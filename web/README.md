# Web Support for impli

This directory contains the web interface for running `impli` in the browser using WebAssembly.

## Current Status

The infrastructure is set up to use [wasm-webterm](https://github.com/cryptool-org/wasm-webterm) for running WASM binaries in the browser. The WASM-compatible executable (`exe/Wasm.hs`) has been created without dependencies on `haskeline` or `terminfo`.

## Building for WASM

### Current Approach (Placeholder)

The current build process creates a native executable as a placeholder:

```bash
./web/build-wasm.sh
```

This builds `impli-wasm` using the standard GHC and places it in `web/static/`.

### Future: Actual WASM Compilation

To build actual WebAssembly binaries, you need to:

1. **Install GHC WASM Backend**
   
   Follow the instructions at [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) to install the WASM cross-compilation toolchain.

2. **Configure for WASM**
   
   Add a `cabal.project.wasm` file:
   ```
   packages: .
   
   package impli
     ghc-options: -optl-Wl,--export-table
   
   with-compiler: wasm32-wasi-ghc
   with-hc-pkg: wasm32-wasi-ghc-pkg
   ```

3. **Build for WASM**
   
   ```bash
   cabal --project-file=cabal.project.wasm build exe:impli-wasm
   ```

4. **Copy WASM binary**
   
   ```bash
   BIN=$(cabal --project-file=cabal.project.wasm list-bin exe:impli-wasm -v0)
   cp "$BIN" web/static/impli.wasm
   ```

## Directory Structure

```
web/
├── build-wasm.sh           # Build script for WASM binary
├── build.sh                # Old JS backend build script (deprecated)
├── lib/
│   ├── emscripten-pty.js   # Old JS backend library (deprecated)
│   └── ffi.js              # Old JS FFI (deprecated)
├── src/
│   └── main.js             # Old JS main (deprecated)
└── static/
    ├── index.html          # Updated for wasm-webterm
    ├── main.js             # New wasm-webterm integration
    ├── style.css           # Terminal styling
    ├── favicon.*           # Favicons
    └── CommitMono*.woff2   # Font files
```

## Testing Locally

To test the web interface locally:

1. Build the WASM binary (or placeholder):
   ```bash
   ./web/build-wasm.sh
   ```

2. Serve the static directory:
   ```bash
   cd web/static
   python3 -m http.server 8000
   ```

3. Open http://localhost:8000 in your browser

4. Type `impli` in the terminal to run the interpreter (once WASM binary is available)

## GitHub Pages Deployment

The GitHub Actions workflow (`.github/workflows/deploy-wasm.yaml`) automatically:

1. Builds the `impli-wasm` executable
2. Copies web assets to the artifact
3. Deploys to GitHub Pages

Access the deployed version at: https://bfeitknecht.github.io/impli/

## Migration from JS Backend

The old JS backend approach (using GHC JS backend + emscripten-pty) has been superseded by this WASM approach for the following reasons:

- **Simpler**: WASM is a more straightforward compilation target
- **Better Integration**: wasm-webterm provides clean integration with xterm.js
- **No Complex Dependencies**: Avoids emscripten versioning issues
- **Standard WASI**: Uses standard WebAssembly System Interface

The old files (build.sh, lib/emscripten-pty.js, lib/ffi.js, src/main.js, exe/Web.hs) are kept for reference but can be removed in a future cleanup.
