{
  description = "Flake for development and build of impli in the web via GHC's JS bachend";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/15ed8f7638116135ac9d3bd4353c482e7c539e0f";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "aarch64-linux";
    version = "ghc912";

    overlay = final: prev:
      let lib = prev.haskell.lib; in {
      haskell = prev.haskell // {
        packageOverrides = self: super: with lib; {
          impli = doJailbreak (self.callCabal2nix "impli" ./. {});
        };
      };
    };

    pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
    ghcjspkgs = pkgs.pkgsCross.ghcjs.haskell.packages.${version};

    cabal-install-ghcjs = pkgs.writeShellScriptBin "javascript-unknown-ghcjs-cabal" ''
      PREFIX=javascript-unknown-ghcjs
      ${pkgs.cabal-install}/bin/cabal \
        --with-compiler=$PREFIX-ghc \
        --with-hc-pkg=$PREFIX-ghc-pkg \
        --with-hsc2hs=$PREFIX-hsc2hs \
        "$@"
    '';

    js-version = pkgs.pkgsCross.ghcjs.haskell.packages.${version};

  in {
    packages.${system}.default = ghcjspkgs.impli;
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        cabal-install-ghcjs
        pkgs.emscripten
        js-version.ghc
        pkgs.nodejs
        pkgs.brotli
      ];
    };
  };
}
