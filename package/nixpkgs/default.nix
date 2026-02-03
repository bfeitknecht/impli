{ lib
, haskell
, haskellPackages
}:

haskellPackages.callCabal2nix "impli" ./.. { }
