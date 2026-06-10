{
  description = "Flake for development and build of impli with WASM support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      ghc-wasm-meta,
      treefmt-nix,
      ...
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      ghcVersion = "ghc9122";
      targetPrefix = "wasm32-wasi-";

      # Conveniences over the raw ghc-wasm-meta package set.
      wasmTools =
        system:
        let
          wasm = ghc-wasm-meta.packages.${system};
        in
        {
          ghc = wasm.wasm32-wasi-ghc-9_12;
          cabal = wasm.wasm32-wasi-cabal-9_12;
          sdk = wasm.wasi-sdk;
          all = wasm.all_9_12;
          postLink = "${wasm.wasm32-wasi-ghc-9_12}/lib/post-link.mjs";
        };

      # Replaces the cross stdenv's CC with the WASM clang toolchain.
      wasmStdenvConfig =
        system:
        let
          wasm = wasmTools system;
        in
        {
          config.replaceCrossStdenv =
            { buildPackages, baseStdenv }:
            buildPackages.stdenvNoCC.override {
              inherit (baseStdenv)
                buildPlatform
                hostPlatform
                targetPlatform
                ;
              cc = wasm.all // {
                isGNU = false;
                isClang = true;
                inherit targetPrefix;
                libc = wasm.sdk.overrideAttrs (attrs: {
                  pname = attrs.name;
                  version = "unstable";
                });
                bintools = wasm.all // {
                  inherit targetPrefix;
                  bintools = wasm.all // {
                    inherit targetPrefix;
                  };
                };
              };
            };
        };

      wasmHaskellOverlay =
        system:
        let
          wasm = wasmTools system;
          nativePkgs = nixpkgs.legacyPackages.${system};
        in
        (final: prev: {
          cabal-install = wasm.cabal;

          haskell =
            (prev.haskell.override (old: {
              buildPackages = nixpkgs.lib.recursiveUpdate old.buildPackages {
                haskell.compiler.${ghcVersion} = wasm.ghc // {
                  inherit targetPrefix;
                };
              };
            }))
            // {
              packageOverrides = nixpkgs.lib.composeManyExtensions [
                prev.haskell.packageOverrides
                (hfinal: hprev: {
                  ghc = wasm.ghc // {
                    inherit (nativePkgs.haskell.packages.${ghcVersion}.ghc) version haskellCompilerName;
                    inherit targetPrefix;
                  };

                  mkDerivation =
                    args:
                    (hprev.mkDerivation (
                      args
                      // {
                        enableLibraryProfiling = false;
                        enableSharedLibraries = true;
                        enableStaticLibraries = false;
                        enableExternalInterpreter = false;
                        doBenchmark = false;
                        doHaddock = false;
                        doCheck = false;
                        jailbreak = true;
                        configureFlags = [
                          "--with-ld=${prev.stdenv.cc.bintools}/bin/lld"
                          "--with-ar=${prev.stdenv.cc.bintools}/bin/ar"
                          "--with-strip=${prev.stdenv.cc.bintools}/bin/strip"
                        ];
                        setupHaskellDepends = (args.setupHaskellDepends or [ ]) ++ [ wasm.sdk ];
                        preBuild = ''
                          ${args.preBuild or ""}
                          export NIX_CC=$CC
                        '';
                      }
                    )).overrideAttrs
                      (attrs: {
                        name = "${attrs.pname}-${targetPrefix}${attrs.version}";
                        preSetupCompilerEnvironment = ''
                          export CC_FOR_BUILD=$CC
                        '';
                      });

                  impli = hfinal.callCabal2nix "impli" ./. { };
                })
              ];
            };
        });

      # Cross pkgs for the given host system, composed from the pieces above.
      wasmPkgs =
        system:
        import nixpkgs (
          {
            inherit system;
            crossSystem = nixpkgs.lib.systems.elaborate nixpkgs.lib.systems.examples.wasi32 // {
              isStatic = false;
            };
            crossOverlays = [ (wasmHaskellOverlay system) ];
          }
          // wasmStdenvConfig system
        );

      # The impli-web WASM derivation for a given host system.
      impliWeb =
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          wasm = wasmTools system;
          hpkgs = (wasmPkgs system).haskell.packages.${ghcVersion};
        in
        hpkgs.impli.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
            pkgs.binaryen
            pkgs.deno
            pkgs.nodejs
          ];
          postInstall = ''
            ${old.postInstall or ""}

            # Ensure the binary has a .wasm extension.
            if [ ! -f $out/bin/impli-web.wasm ] && [ -f $out/bin/impli-web ]; then
              cp $out/bin/impli-web $out/bin/impli-web.wasm
            fi
            if [ ! -f $out/bin/impli-web.wasm ]; then
              echo "ERROR: impli-web.wasm not found in $out/bin/"
              ls -la $out/bin/ || true
              exit 1
            fi

            # Optimise with wasm-opt.
            echo "Running wasm-opt..."
            wasm-opt -Os $out/bin/impli-web.wasm -o $out/bin/impli.wasm

            # Generate the GHC JS stub.
            echo "Generating stub.js..."
            mkdir -p $out/web
            ${wasm.postLink} -i $out/bin/impli.wasm -o $out/web/stub.js
          '';
        });
    in
    {
      packages = forAllSystems (system: {
        impli-web = impliWeb system;
        default = impliWeb system;
      });

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          buildScript = pkgs.writeShellScriptBin "build" ''
            set -euo pipefail

            if [ ! -d "web" ]; then
              echo "Error: 'web' directory not found. Please run this from the project root."
              exit 1
            fi

            echo "Copying WASM and stub to web/public..."
            mkdir -p web/public
            cp "${impliWeb system}/bin/impli.wasm" web/public/impli.wasm
            cp "${impliWeb system}/web/stub.js" web/public/stub.js
            chmod +w web/public/impli.wasm web/public/stub.js

            echo "Running deno task build..."
            cd web
            ${pkgs.deno}/bin/deno task build
            echo "Done!"
          '';
        in
        {
          build = {
            type = "app";
            program = "${buildScript}/bin/build";
          };
          default = self.apps.${system}.build;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.python3
              pkgs.fourmolu
              pkgs.brotli
              pkgs.deno
              (wasmTools system).all
            ];
            shellHook = ''
              echo "impli dev shell (${system})"
              echo "  nix run .#build   -- build WASM output, copy to web/public, and run Deno build"
            '';
          };
        }
      );

      formatter = forAllSystems (
        system:
        (treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
        }).config.build.wrapper
      );
    };
}
