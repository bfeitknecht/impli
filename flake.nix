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
    
  in {
    packages.${system} = {
      # Default package builds the web version
      default = self.packages.${system}.impli-web;
      
      # Web version built with WASM backend
      # Uses the ghc-wasm-meta package which provides all_9_12 with WASM cross-compilation support
      impli-web = ghc-wasm-meta.packages.${system}.all_9_12;
    };
    
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        ghc-wasm-meta.packages.${system}.all_9_12
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
