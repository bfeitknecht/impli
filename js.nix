{ version ? "ghc912"
}:

let
  pins = {
    # merge of https://github.com/NixOS/nixpkgs/pull/444862
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/15ed8f7638116135ac9d3bd4353c482e7c539e0f.tar.gz";
      sha256 = "sha256:00ypnmxqm216jw55gvrh64v7shadzr16ppp3c7qpbxlkiq0mdars";
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
