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
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
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
    in
    {
      # Restricted to x86_64-linux for binary builds.
      # We use stdenv.mkDerivation to bypass the broken Nixpkgs Haskell cross-compilation infrastructure
      # (specifically the libffi emscripten header error) by using the pre-built toolchain from ghc-wasm-meta.
      packages.x86_64-linux =
        let
          system = "x86_64-linux";
          pkgs = import nixpkgs { inherit system; };

          # This toolchain is pre-built and cached at ghc-wasm-meta.cachix.org
          wasmTools = ghc-wasm-meta.packages.${system}.all_9_12;
        in
        {
          impli-web = pkgs.stdenv.mkDerivation {
            pname = "impli-web";
            version = "4.0.0.0";
            src = ./.;

            # We only need wasmTools. It includes its own cabal-install wrapper.
            # Using pkgs.haskell.packages.*.cabal-install would trigger a heavy rebuild of the Haskell world.
            nativeBuildInputs = [ wasmTools ];

            buildPhase = ''
              export HOME=$TMPDIR
              # wasm32-wasi-cabal is a wrapper that handles the GHC WASM backend automatically.
              # We use --project-file=cabal.project to ensure we pick up local configuration.
              wasm32-wasi-cabal build impli-web --project-file=cabal.project
            '';

            installPhase = ''
              mkdir -p $out/bin
              # Find the resulting binary and copy it as a .wasm file
              find dist-newstyle -name "impli-web" -type f -exec cp {} $out/bin/impli-web.wasm \;
              # Create a symlink without extension for common tooling
              ln -s $out/bin/impli-web.wasm $out/bin/impli-web
            '';
          };

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
              - cabal (native Haskell builds)
              ${
                if wasmTools != null then
                  "- wasm32-wasi-cabal (WASM builds)\n  - wasm32-wasi-ghc (WASM GHC)"
                else
                  "- [Note] WASM toolchain (ghc-wasm-meta) not available for this platform"
              }
              - fourmolu (code formatter)
              - node (Node.js)
              - python (Python 3)

            To build via Nix (x86_64-linux only):
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
