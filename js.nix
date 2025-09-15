{ version ? "ghc912"
}:

let
  pins = {
    # merge of https://github.com/NixOS/nixpkgs/pull/438642
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/1779f9e0d8b45d88d7525665dd4d2a5b65041248.tar.gz";
      sha256 = "sha256:0ik0r3mxw1nn7089grzz15i9hmxfdprbjwi5siy2xqkb4wg67n2w";
    };
  };

  src = nixpkgs.nix-gitignore.gitignoreSourcePure [
    ''
      *.nix
    ''
    ./.gitignore
  ] ./.;

  nixpkgs = import pins.nixpkgs {
    config = {
      packageOverrides = nixpkgs: {
        haskell = nixpkgs.haskell // {
          packageOverrides = self: super: with nixpkgs.haskell.lib; {
            impli = doJailbreak (self.callCabal2nix "impli" src {});
          };
        };
      };
    };
  };

  cabal-install-ghcjs = nixpkgs.writeShellScriptBin "javascript-unknown-ghcjs-cabal" ''
    PREFIX=javascript-unknown-ghcjs
    ${nixpkgs.cabal-install}/bin/cabal \
      --with-compiler=$PREFIX-ghc \
      --with-hc-pkg=$PREFIX-ghc-pkg \
      --with-hsc2hs=$PREFIX-hsc2hs \
      "$@"
  '';

  js-version = nixpkgs.pkgsCross.ghcjs.haskell.packages.${version};

in {
  inherit nixpkgs;
  build = js-version.impli;
  shell = with nixpkgs; mkShell {
    packages = [
      cabal-install-ghcjs
      emscripten
      nodejs
      js-version.ghc
    ];
  };
}
