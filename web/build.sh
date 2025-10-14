#!/usr/bin/env bash

set -euf -o pipefail

echo "+++ INFO: update cabal"
javascript-unknown-ghcjs-cabal update

echo "+++ INFO: build target exe:impli-web"
javascript-unknown-ghcjs-cabal build exe:impli-web

echo "+++ INFO: minimize generated JS"
BIN="$(javascript-unknown-ghcjs-cabal list-bin exe:impli-web -v0).jsexe"
OUT="./web/static"
# TODO: ensure swc does not mangle ad absurdium
# swc bundle "$BIN"/all.js -o "$OUT"/impli.js
cp "$BIN"/all.js "$OUT"/impli.js
du -ahx "$OUT"/impli.js

# TODO: ensure deno installed in builder
# echo "+++ INFO: bundle everything"
# deno bundle \
#   --minify \
#   --platform=browser \
#   --external=xterm \
#   --external=xterm-pty \
#   --external=xterm-addon-fit \
#   --external=./impli.js \
#   web/src/main.js -o web/static/module.mjs
