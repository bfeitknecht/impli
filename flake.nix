{
  description = "Flake for development and build of impli with WASM support (x86_64-linux only)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ghc-wasm-meta, ... }:

  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };

    # Toolchain providing wasm32-wasi-cabal, wasm32-wasi-ghc, etc.
    wasmPkgs = ghc-wasm-meta.packages.${system}.all_9_12;

    shellMessage = ''
      ======================================
      impli WASM development environment (x86_64-linux)
      ======================================

      Available tools:
        - cabal (native Haskell builds)
        - wasm32-wasi-cabal (WASM builds)
        - wasm32-wasi-ghc (WASM GHC)
        - fourmolu (code formatter)
        - node (Node.js)
        - python (Python 3)

      To build the WASM binary manually:
        wasm32-wasi-cabal build impli-web

      To build via Nix:
        nix build .#impli-web
        (The output binary will be in result/bin/)
    '';
  in {
    packages.${system} = {
      # ERROR: this uses the incorrect version of cabal, use the with GHC WASM backend (from ghc-wasm-meta?)
      impli-web = pkgs.haskellPackages.callCabal2nix "impli-web" ./. {};
      default = self.packages.${system}.impli-web;
    };

    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        wasmPkgs
        pkgs.python3
        pkgs.fourmolu
        pkgs.brotli
      ];

      shellHook = "echo '${shellMessage}'";
    };
  };
}
