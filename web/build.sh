#!/usr/bin/env bash

set -euf -o pipefail

artifacts=dist-newstyle
echo "+++ INFO: clean up artifacts"
rm -rf $artifacts

JSCABAL=javascript-unknown-ghcjs-cabal
echo "+++ INFO: update cabal"
$JSCABAL update

echo "+++ INFO: build target exe:impli-web"
$JSCABAL build exe:impli-web

echo "+++ INFO: minimize generated JS"
BIN="$($JSCABAL list-bin exe:impli-web -v0).jsexe"
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
