{
  description = "Flake for development and build of impli with WASM support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ghc-wasm-meta, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    
    # Get WASM cross-compilation toolchain
    wasmPkgs = ghc-wasm-meta.packages.${system};
    
  in {
    packages.${system} = {
      # Default package builds the web version
      default = self.packages.${system}.impli-web;
      
      # Web version built with WASM backend
      impli-web = wasmPkgs.buildPackages.haskell.packages.ghc-wasm32.callCabal2nix "impli" ./. {};
    };
    
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        wasmPkgs.all_9_12
        pkgs.cabal-install
        pkgs.nodejs
        pkgs.brotli
        pkgs.python3
      ];
      
      shellHook = ''
        echo "WASM development environment loaded"
        echo "Build with: wasm32-wasi-cabal build exe:impli-web"
      '';
    };
  };
}
