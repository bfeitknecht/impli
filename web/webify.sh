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

BIN="$(wasm32-wasi-cabal list-bin impli-wasm -v0 --project-file=cabal.project.wasm | sed 's#opt/##')"
WASM="./web/impli.wasm"

echo "INFO: COPYING $BIN TO $WASM"
cp "$BIN" "$WASM"

GLUE="./web/glue.js"

echo "INFO: CREATING JSFFI MODULE $GLUE"
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i "$WASM" -o "$GLUE"
