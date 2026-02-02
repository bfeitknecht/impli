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
      inherit (nixpkgs) lib;
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: lib.genAttrs supportedSystems (system: f system);
      
      ghc = "ghc9122";
      targetPrefix = "wasm32-wasi-";
      
      # Create WASM-enabled Haskell packages using the approach from nix-wasm
      # Only available on x86_64-linux
      wasmPkgs = system: import nixpkgs rec {
        inherit system;
        crossSystem = lib.systems.elaborate lib.systems.examples.wasi32 // {
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
            libc = ghc-wasm-meta.packages.${system}.wasi-sdk.overrideAttrs (attrs: { pname = attrs.name; version = "unstable1"; });
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
              buildPackages = lib.recursiveUpdate old.buildPackages {
                haskell.compiler.${ghc} = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc-9_12 // {
                  inherit targetPrefix;
                };
              };
            })) // {
              packageOverrides = lib.composeManyExtensions [
                prev.haskell.packageOverrides
                (hfinal: hprev: {
                  ghc = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc-9_12 // {
                    inherit (nixpkgs.legacyPackages.${system}.haskell.packages.${ghc}.ghc) version haskellCompilerName;
                    inherit targetPrefix;
                  };
                  mkDerivation = args: (hprev.mkDerivation (args // {
                    enableLibraryProfiling = false;
                    enableSharedLibraries = false;
                    enableStaticLibraries = false;
                    enableExternalInterpreter = false;
                    doBenchmark = false;
                    doHaddock = false;
                    doCheck = false;
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
    {
      # WASM packages only for x86_64-linux
      packages.x86_64-linux =
        let
          system = "x86_64-linux";
          wasmPackages = wasmPkgs system;
          haskellPackages = wasmPackages.haskell.packages.${ghc};
        in
        {
          impli-web = haskellPackages.impli;
          
          default = self.packages.x86_64-linux.impli-web;
        };

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

            WASM build via Nix (only on x86_64-linux with ghc-wasm-meta support):
              ${
                if system == "x86_64-linux" then
                  "nix build .#impli-web"
                else
                  "[Not available on ${system}]"
              }
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
