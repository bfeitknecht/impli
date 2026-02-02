{
  description = "Flake for development and build of impli with WASM support";

  nixConfig = {
    extra-substituters = [ "https://ghc-wasm-meta.cachix.org" ];
    extra-trusted-public-keys = [
      "ghc-wasm-meta.cachix.org-1:6C9S967v6XvAizKovS/I844z6o0qR2r3/2DkY7vXGgY="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      ghc-wasm-meta,
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
      
      ghc = "ghc9122";
      targetPrefix = "wasm32-wasi-";
      
      # Create WASM-enabled Haskell packages using the approach from nix-wasm
      wasmPkgs = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            crossSystem = nixpkgs.lib.systems.elaborate nixpkgs.lib.systems.examples.wasi32 // {
              isStatic = false;
            };
            config.replaceCrossStdenv = { buildPackages, baseStdenv }: buildPackages.stdenvNoCC.override {
              inherit (baseStdenv)
                buildPlatform
                hostPlatform
                targetPlatform;
              cc = ghc-wasm-meta.packages.${system}.all_9_12 // {
                isGNU = false;
                isClang = true;
                libc = ghc-wasm-meta.packages.${system}.wasi-sdk.overrideAttrs (attrs: { pname = attrs.name; version = "unstable"; });
                inherit targetPrefix;
                bintools = ghc-wasm-meta.packages.${system}.all_9_12 // {
                  inherit targetPrefix;
                  bintools = ghc-wasm-meta.packages.${system}.all_9_12 // {
                    inherit targetPrefix;
                  };
                };
              };
            };
            crossOverlays = [
              (final: prev: {
                cabal-install = ghc-wasm-meta.packages.${system}.wasm32-wasi-cabal-9_12;
                haskell = (prev.haskell.override (old: {
                  buildPackages = nixpkgs.lib.recursiveUpdate old.buildPackages {
                    haskell.compiler.${ghc} = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc-9_12 // {
                      inherit targetPrefix;
                    };
                  };
                })) // {
                  packageOverrides = nixpkgs.lib.composeManyExtensions [
                    prev.haskell.packageOverrides
                    (hfinal: hprev: {
                      ghc = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc-9_12 // {
                        inherit (nixpkgs.legacyPackages.${system}.haskell.packages.${ghc}.ghc) version haskellCompilerName;
                        inherit targetPrefix;
                      };
                      mkDerivation = args: (hprev.mkDerivation (args // {
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
                        setupHaskellDepends = (args.setupHaskellDepends or [ ]) ++ [
                          ghc-wasm-meta.packages.${system}.wasi-sdk
                        ];
                        preBuild = ''
                          ${args.preBuild or ""}
                          export NIX_CC=$CC
                        '';
                      })).overrideAttrs (attrs: {
                        name = "${attrs.pname}-${targetPrefix}${attrs.version}";
                        preSetupCompilerEnvironment = ''
                          export CC_FOR_BUILD=$CC
                        '';
                      });
                      # Package-specific overrides
                      impli = hfinal.callCabal2nix "impli" ./. { };
                    })
                  ];
                };
              })
            ];
          };
        in
        pkgs;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          wasmPackages = wasmPkgs system;
          haskellPackages = wasmPackages.haskell.packages.${ghc};
        in
        {
          impli-web = haskellPackages.impli.overrideAttrs (old: {
            postInstall = ''
              ${old.postInstall or ""}
              # Copy the WASM binary with the expected name
              if [ -f $out/bin/impli-web.wasm ]; then
                true
              elif [ -f $out/bin/impli-web ]; then
                cp $out/bin/impli-web $out/bin/impli-web.wasm
              else
                echo "Warning: impli-web binary not found at expected locations ($out/bin/impli-web.wasm or $out/bin/impli-web)"
                echo "Build may have failed or binary name may have changed. Contents of $out/bin/:"
                ls -la $out/bin/ || true
              fi
            '';
          });
          
          default = self.packages.${system}.impli-web;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          wasmTools = ghc-wasm-meta.packages.${system}.all_9_12 or null;

          shellMessage = ''
            ======================================
            impli WASM development environment (${system})
            ======================================

            Available tools:
              ${
                if wasmTools != null then
                  "- wasm32-wasi-cabal (WASM builds)\n  - wasm32-wasi-ghc (WASM GHC)"
                else
                  "- [Note] WASM toolchain (ghc-wasm-meta) not available for this platform"
              }
              - fourmolu (code formatter)
              - node (Node.js)
              - python (Python 3)

            To build via Nix:
              nix build .#impli-web
          '';
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.python3
              pkgs.fourmolu
              pkgs.brotli
              pkgs.nodejs
            ]
            ++ (if wasmTools != null then [ wasmTools ] else [ ]);

            shellHook = "echo '${shellMessage}'";
          };
        }
      );
    };
}
