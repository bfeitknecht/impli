{
  description = "Build impli for the web via GHC's JS bachend";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/1779f9e0d8b45d88d7525665dd4d2a5b65041248";
  };

  outputs = { self, nixpkgs }:
  let
    system = "aarch64-linux";
    version = "ghc912";

    custom = final: prev:
      let lib = prev.haskell.lib; in {
      haskell = prev.haskell // {
        packageOverrides = self: super: with lib; {
          impli = doJailbreak (self.callCabal2nix "impli" ./. {});
        };
      };
    };

    pkgs = import nixpkgs { inherit system; overlays = [ custom ]; };
    jsPkgs = pkgs.pkgsCross.ghcjs.haskell.packages.${version};

  in {
    defaultPackage = jsPkgs.impli;
    packages = {
      "${system}" = { default = jsPkgs.impli; };
    };
  };
}
