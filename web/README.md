# Web Support for impli

This directory contains the web interface for running `impli` in the browser using WebAssembly.

## Architecture

The web version uses [wasm-webterm](https://github.com/cryptool-org/wasm-webterm) to provide a terminal interface that can execute WASM binaries compiled with WASI support.

```
Browser → XTerm.js + wasm-webterm → impli.wasm (WASI) → Basic IO REPL
```

## Implementation

### WASM-Compatible Executable

**File**: `exe/Web.hs` (and supporting modules in `exe/REPL/`)

The web version uses a polymorphic REPL architecture:
- `exe/REPL/State.hs`: Core REPL state and helper functions
- `exe/REPL/Execute.hs`: Haskeline-based REPL for CLI (uses `InputT IO`)
- `exe/REPL/Web.hs`: Basic IO REPL for WASM (uses plain `IO`)

The `REPL.Web` module implements the full REPL without dependencies on `haskeline` or `terminfo`, making it WASI-compatible. Both implementations share the same state management and helper functions via the polymorphic `REPL m` monad.

### Web Infrastructure

**Files in `web/static/`**:
- `index.html` - Terminal interface using XTerm.js + wasm-webterm
- `main.js` - Integration code that auto-launches impli.wasm
- `style.css` - Terminal styling
- `impli.wasm` - The WASM binary (generated during build)

## Building for WASM

### Using the Nix Flake (Recommended)

The repository provides a Nix flake with a development shell that includes all necessary tools for WASM compilation:

```bash
# Enter the development shell
nix develop

# Build the WASM executable
nix build .#impli-web

# The WASM binary will be in result/bin/impli-web
cp result/bin/impli-web web/static/impli.wasm
```

### Manual Build (Without Nix)

If you have the GHC WASM backend installed separately:

```bash
# Build with the WASM backend
wasm32-wasi-cabal build exe:impli-web

# Copy to web directory
BIN=$(wasm32-wasi-cabal list-bin exe:impli-web -v0)
cp "$BIN" web/static/impli.wasm
```

## Testing Locally

To test the web interface locally:

1. Build the WASM binary (see above)

2. Serve the static directory:
   ```bash
   cd web/static
   python3 -m http.server 8000
   ```

3. Open http://localhost:8000 in your browser

4. The impli REPL will automatically launch in the terminal

## GitHub Pages Deployment

The GitHub Actions workflow (`.github/workflows/deploy-github-pages.yaml`) automatically:

1. Uses the Nix flake to build `impli-web` with the WASM backend
2. Copies the WASM binary and web assets
3. Copies example IMP programs from `docs/examples/`
4. Deploys to GitHub Pages

Access the deployed version at: https://bfeitknecht.github.io/impli/

## Directory Structure

```
web/
├── build.sh                # Old JS backend build script (deprecated)
├── lib/                    # Old JS backend files (deprecated)
├── src/                    # Old JS backend files (deprecated)
├── static/
│   ├── index.html          # Terminal UI (wasm-webterm)
│   ├── main.js             # Auto-launches impli.wasm
│   ├── style.css           # Terminal styling
│   ├── impli.wasm          # WASM binary (build artifact)
│   ├── *.imp               # Example programs (copied from docs/examples/)
│   └── fonts/              # Fonts and favicons
└── README.md               # This file
```

## Migration from JS Backend

The previous approach using the GHC JavaScript backend with emscripten-pty has been superseded by this WASM approach for several reasons:

- **Simpler**: WASM is a more straightforward compilation target
- **Better Integration**: wasm-webterm provides clean integration with xterm.js  
- **No Complex Dependencies**: Avoids emscripten versioning issues
- **Standard WASI**: Uses the WebAssembly System Interface standard

Old files (`build.sh`, `lib/`, `src/`) are kept for reference but can be removed in a future cleanup.

## Resources

- **wasm-webterm**: https://github.com/cryptool-org/wasm-webterm
- **GHC WASM Backend**: https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
- **Nix Flake**: See `flake.nix` in the repository root
- **WASI Specification**: https://wasi.dev/
- **XTerm.js**: https://xtermjs.org/
