## `impli` Web

The `impli` REPL as a [Preact](https://preactjs.com) web app, running the Haskell interpreter entirely client-side via WebAssembly.
Built with [Deno](https://deno.com) and deployed to [GitHub Pages](https://bfeitknecht.github.io/impli).

### Architecture

The app is a static site with no server-side logic.
The Haskell interpreter is cross-compiled to a WASM binary (`impli.wasm`) and executed in the browser inside a terminal emulator.

```
┌─────────────┐     JSFFI (stub.js)     ┌────────────────┐
│  impli.wasm │ ◄──────────────────────► │  Impli class   │
│  (Haskell)  │                          │  (TypeScript)  │
└──────┬──────┘                          └───────┬────────┘
       │ stdin/stdout/stderr                     │
       ▼                                         ▼
┌──────────────┐                         ┌────────────────┐
│  @runno/wasi │                         │   xterm.js     │
│  (WASI shim) │                         │   (terminal)   │
└──────────────┘                         └────────────────┘
```

- **`impli.wasm`**, Haskell interpreter compiled to `wasm32-wasi` via GHC 9.12's WASM backend
- **`stub.js`**, auto-generated JSFFI bridge (from GHC's `post-link.mjs`) mapping Haskell FFI calls to JavaScript callbacks on the `Impli` instance
- **`@runno/wasi`**, in-browser WASI runtime providing a virtual filesystem (populated with example `.imp` files) and stdio routing
- **`xterm.js`**, terminal emulator with fit and local-echo addons for line editing, history, and tab-completion

WASM instantiation uses a knot-tying pattern where an empty `exports` object is passed to `stub()` during instantiation, then backfilled with the actual instance exports before calling `start()`.

### Build

Building is a two-stage process.

**1. WASM binary** (from the repo root):

```sh
nix build .#impli-web
cp result/bin/impli-web.wasm web/public/impli.wasm
cp result/web/stub.js web/public/stub.js
```

**2. Frontend bundle** (from `web/`):

```sh
deno install
deno task build
```

This runs `build.ts`, which generates `examples.js` (a virtual filesystem of `.imp` files from `docs/examples/`) and bundles the Preact app into `module.mjs`.

Both stages output to `web/public/`, which is ignored by Git and populated at build time.
The `web/static/` directory contains only committed assets (HTML, CSS, fonts, favicon) which the deploy workflow copies into `web/public/` before uploading.

### Deployment

The GitHub Actions workflow (`.github/workflows/deploy.yaml`) runs both stages on every push to `master`, copies `web/static/` into `web/public/`, and deploys the contents of `web/public/` to GitHub Pages.
