# Instructions for Full WASM Compilation

This document provides step-by-step instructions for compiling `impli` to WebAssembly using the GHC WASM backend.

## Prerequisites

1. **GHC WASM Toolchain**
   
   The GHC WASM backend is available from the [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) project.
   
   Installation options:
   - Via ghcup (recommended when available)
   - Manual installation from releases
   - Building from source

2. **wasm32-wasi Target Tools**
   
   The WASM backend requires wasm32-wasi target tools. These are typically included with the ghc-wasm-meta installation.

## Build Steps

### 1. Verify WASM GHC Installation

```bash
wasm32-wasi-ghc --version
wasm32-wasi-cabal --version
```

### 2. Create WASM-Specific Cabal Project File

Create `cabal.project.wasm`:

```
packages: .

-- Configure for WASM target
with-compiler: wasm32-wasi-ghc
with-hc-pkg: wasm32-wasi-ghc-pkg

package impli
  -- Export symbols needed by WASI
  ghc-options: -optl-Wl,--export-table
  
  -- Optimization flags
  ghc-options: -O2
```

### 3. Build the WASM Executable

```bash
# Update Cabal package index
wasm32-wasi-cabal update

# Build the WASM executable
wasm32-wasi-cabal --project-file=cabal.project.wasm build exe:impli-wasm

# Locate the built WASM file
BIN=$(wasm32-wasi-cabal --project-file=cabal.project.wasm list-bin exe:impli-wasm -v0)
echo "WASM binary location: $BIN"
```

### 4. Copy WASM Binary to Web Directory

```bash
# Copy the WASM binary
cp "$BIN" web/static/impli.wasm

# Verify it's a WASM file
file web/static/impli.wasm
# Should output: web/static/impli.wasm: WebAssembly (wasm) binary module version 0x1 (MVP)
```

### 5. Test Locally

```bash
# Serve the web directory
cd web/static
python3 -m http.server 8000

# Open http://localhost:8000 in your browser
# Type 'impli' in the terminal to run the interpreter
```

## Optimizing WASM Binary Size

The WASM binary can be quite large. Here are some optimization techniques:

### 1. Strip Debug Information

```bash
wasm-strip web/static/impli.wasm
```

### 2. Optimize with wasm-opt

```bash
# Install Binaryen tools if not already installed
# On macOS: brew install binaryen
# On Ubuntu: apt-get install binaryen

# Optimize for size
wasm-opt -Os web/static/impli.wasm -o web/static/impli.wasm

# Or optimize for speed
wasm-opt -O3 web/static/impli.wasm -o web/static/impli.wasm
```

### 3. Compress for Deployment

```bash
# Gzip compression
gzip -9 -k web/static/impli.wasm

# Brotli compression (better compression)
brotli -9 web/static/impli.wasm
```

## Troubleshooting

### Issue: "Module not found" Error

If you see a "Module not found" error in the browser console, ensure:
1. The WASM file is named exactly `impli.wasm`
2. It's in the same directory as `index.html`
3. Your web server is configured to serve `.wasm` files with the correct MIME type (`application/wasm`)

### Issue: "RuntimeError: memory access out of bounds"

This can happen if the WASM module's memory is too small. Try:
1. Increasing the initial memory size in the build flags
2. Enabling memory growth in the WASM configuration

### Issue: Large Binary Size

If the WASM binary is very large (>10MB):
1. Ensure you're building with optimizations (`-O2`)
2. Strip debug symbols with `wasm-strip`
3. Use `wasm-opt` to further optimize
4. Consider compressing the binary with gzip or brotli

## CI/CD Integration

To integrate WASM compilation into GitHub Actions:

1. Install the WASM toolchain in the workflow
2. Build with `wasm32-wasi-cabal`
3. Upload the WASM binary as an artifact
4. Deploy to GitHub Pages

Example workflow step:

```yaml
- name: Install GHC WASM
  run: |
    # Install ghc-wasm-meta
    # (Instructions depend on available installation method)
    
- name: Build WASM
  run: |
    wasm32-wasi-cabal --project-file=cabal.project.wasm build exe:impli-wasm
    BIN=$(wasm32-wasi-cabal --project-file=cabal.project.wasm list-bin exe:impli-wasm -v0)
    cp "$BIN" web/static/impli.wasm
```

## References

- [GHC WASM Backend](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- [wasm-webterm Documentation](https://github.com/cryptool-org/wasm-webterm)
- [WebAssembly System Interface (WASI)](https://wasi.dev/)
- [Binaryen Tools](https://github.com/WebAssembly/binaryen)
