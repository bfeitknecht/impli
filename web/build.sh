#!/usr/bin/env bash

set -euf -o pipefail

echo "+++ INFO: update cabal"
javascript-unknown-ghcjs-cabal update

echo "+++ INFO: build target exe:impli-web"
javascript-unknown-ghcjs-cabal build exe:impli-web

echo "+++ INFO: minimize generated JS"
BIN="$(javascript-unknown-ghcjs-cabal list-bin -v0 exe:impli-web).jsexe"
OUT="./web/static"
# npx swc bundle "$BIN"/all.js -o "$OUT"/impli.js
# TODO: .swcrc?
cp "$BIN"/all.js "$OUT"/impli.js

# TODO: deno bundle src/main.js > static/module.mjs
