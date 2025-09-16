{
  description = "Build impli for the web via GHC-JS";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/1779f9e0d8b45d88d7525665dd4d2a5b65041248";
  };

  outputs = { self, nixpkgs }:
  let
    system = "aarch64-linux";

    src = ./.;

    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (final: prev:
          let lib = prev.haskell.lib; in {
            haskell = prev.haskell // {
              packageOverrides = self: super: with lib; {
                impli = doJailbreak
                  (self.callCabal2nix "impli" src {});
              };
            };
          })
      ];
    };

    jsPkgs = pkgs.pkgsCross.ghcjs.haskell.packages.ghc912;

  in {
    defaultPackage = jsPkgs.impli;
    packages = {
      "${system}" = {
        default = jsPkgs.impli;
      };
    };
  };
}
