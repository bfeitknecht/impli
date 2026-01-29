{
  description = "Flake for development and build of impli";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        # Get the GHC version from nixpkgs
        ghc = pkgs.haskell.compiler.ghc948;
        
        # Haskell package set for building the project
        haskellPackages = pkgs.haskell.packages.ghc948;
        
        # Build the impli package using cabal2nix
        impli = haskellPackages.callCabal2nix "impli" ./. {};
        
      in {
        packages = {
          # Default package builds the native binary
          default = self.packages.${system}.impli;
          
          # Native impli executable
          impli = impli;
        };
        
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Haskell toolchain
            ghc
            pkgs.cabal-install
            
            # Development tools
            pkgs.fourmolu        # Code formatter
            pkgs.haskell-language-server  # LSP server for IDE support
            
            # Build tools
            pkgs.zlib
          ];
          
          shellHook = ''
            echo "======================================"
            echo "impli Development Environment"
            echo "======================================"
            echo ""
            echo "Available tools:"
            echo "  - ghc ${ghc.version}"
            echo "  - cabal-install (for building)"
            echo "  - fourmolu (for code formatting)"
            echo "  - haskell-language-server (for IDE support)"
            echo ""
            echo "Quick start:"
            echo "  cabal build        # Build the project"
            echo "  cabal test         # Run tests"
            echo "  cabal run impli    # Run the REPL"
            echo "  fourmolu -i .      # Format code"
            echo ""
          '';
        };
      }
    );
}
