#!/usr/bin/env bash

set -euf -o pipefail

cabby="javascript-unknown-ghcjs-cabal"

# echo "+++ INFO: update cabal"
# $cabby update

echo "+++ INFO: build target exe:impli-web"
$cabby build exe:impli-web

echo "+++ INFO: minimize generated JS"
BIN="$($cabby list-bin -v0 exe:impli-web).jsexe"
OUT="./web/static"
# npx swc bundle "$BIN"/all.js -o "$OUT"/impli.js
# TODO: .swcrc?
cp "$BIN"/all.js "$OUT"/impli.js

# TODO: deno bundle src/main.js > static/module.mjs
