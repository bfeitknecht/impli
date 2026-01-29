#!/usr/bin/env bash

# Build script for impli WASM support
# This script builds the impli-wasm executable and prepares it for web deployment

set -euf -o pipefail

echo "+++ Building impli-wasm executable"
cabal build exe:impli-wasm

echo "+++ Locating binary"
BIN=$(cabal list-bin exe:impli-wasm -v0)

echo "+++ Binary location: $BIN"

# For now, copy the native binary to web/static for testing
# TODO: Replace with actual WASM compilation when GHC WASM backend is available
echo "+++ Copying binary to web/static (placeholder for WASM)"
mkdir -p web/static
cp "$BIN" web/static/impli.wasm.placeholder

echo "+++ Build complete!"
echo ""
echo "Note: This currently builds a native executable."
echo "To build actual WASM, you need:"
echo "  1. Install ghc-wasm-meta toolchain"
echo "  2. Configure cabal to use wasm32-wasi-ghc"
echo "  3. Build with: wasm32-wasi-cabal build exe:impli-wasm"
