#!/usr/bin/env bash

set -euf -o pipefail

echo "+++ INFO: update cabal"
javascript-unknown-ghcjs-cabal update

echo "+++ INFO: build target exe:impli-web"
javascript-unknown-ghcjs-cabal build exe:impli-web

echo "+++ INFO: minimize generated JS"
BIN="$(javascript-unknown-ghcjs-cabal list-bin exe:impli-web -v0).jsexe"
OUT="./web/static"
# swc bundle "$BIN"/all.js -o "$OUT"/impli.js # TODO: .swcrc?
cp "$BIN"/all.js "$OUT"/impli.js
du -ahx "$OUT"/impli.js

# TODO: deno bundle src/main.js > static/module.mjs
