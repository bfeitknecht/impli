{
  description = "Flake for development and build of impli with WASM support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ghc-wasm-meta, ... }:
  let
    # NOTE: Currently hard-coded to x86_64-linux for WASM cross-compilation
    # Multi-platform support can be added in the future if needed
    # WASM compilation requires specific cross-compilation tools only available on x86_64-linux
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    
    # Get the WASM development environment (includes wasm32-wasi-cabal and other tools)
    wasmPkgs = ghc-wasm-meta.packages.${system}.all_9_12;
    
    # Get the WASM Haskell package set for building Haskell packages
    # This is separate from the development tools and provides callCabal2nix
    wasmHaskellPkgs = ghc-wasm-meta.packages.${system}.ghc-wasm32-wasi;
    
  in {
    packages.${system} = {
      # Default package builds the WASM binary
      default = self.packages.${system}.impli-web;
      
      # Native impli CLI binary
      impli = pkgs.haskellPackages.callCabal2nix "impli" ./. {};
      
      # Web version built with WASM backend
      # Uses Nix's Haskell infrastructure to properly handle all dependencies
      impli-web = wasmHaskellPkgs.callCabal2nix "impli" ./. {};
    };
    
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        wasmPkgs
        pkgs.cabal-install
        pkgs.nodejs
        pkgs.brotli
        pkgs.python3
      ];
      
      shellHook = ''
        echo "======================================"
        echo "impli WASM development environment"
        echo "======================================"
        echo ""
        echo "Available tools:"
        echo "  - cabal (native Haskell builds)"
        echo "  - wasm32-wasi-cabal (WASM builds)"
        echo "  - wasm32-wasi-ghc (WASM GHC)"
        echo "  - node (Node.js for testing)"
        echo ""
        echo "Build WASM binary:"
        echo "  wasm32-wasi-cabal build exe:impli-web"
        echo ""
        echo "Or use nix build:"
        echo "  nix build . # Builds impli.wasm to result/bin/"
        echo ""
      '';
    };
  };
}
