#!/usr/bin/env bash
#
# run from inside
# (orb -m nixe nix shell
#     'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'
#     nixpkgs#wabt
#     --extra-experimental-features nix-command
#     --extra-experimental-features flakes)

set -euf -o pipefail

echo "INFO: STARTING BUILD"
wasm32-wasi-cabal build impli-wasm --project-file=cabal.project.wasm --ghc-options=-no-hs-main

BIN="$(wasm32-wasi-cabal list-bin impli-wasm -v0 | sed 's#opt/##')"
WEB="./web"

echo "INFO: COPYING TO $WEB"
cp "$BIN" "$WEB/impli.wasm"
