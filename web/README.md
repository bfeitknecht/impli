## `impli` Web

The `impli` REPL as a [Preact](https://preactjs.com) web app, running the
Haskell interpreter entirely client-side via WebAssembly. Built with
[Deno](https://deno.com) and deployed to
[GitHub Pages](https://bfeitknecht.github.io/impli).

### Architecture

The app is a static site with no server-side logic. The Haskell interpreter is
cross-compiled to a WASM binary (`impli.wasm`) and driven by the JavaScript
frontend through an export-only JSON protocol: the browser never leaves the
interpreter in charge of stdin/stdout, the JavaScript side drives prompt,
input and all metacommands.

```
┌─────────────┐   ---------- exports -----------   ┌────────────────┐
│  impli.wasm │ ◄ evaluate/resolveInput/ ...  ──►  │  Impli class   │
│  (Haskell)  │   ◄──────── JSON envelopes ─────   │  (TypeScript)  │
└──────┬──────┘                                    └───────┬────────┘
       │                                                   │
       ▼                                                   ▼
┌──────────────┐                                   ┌────────────────┐
│  @runno/wasi │                                   │  ghostty-web   │
│  (WASI shim) │                                   │   (terminal)   │
└──────────────┘                                   └────────────────┘
```

- **`impli.wasm`**, Haskell interpreter compiled to `wasm32-wasi` via GHC
  9.12's WASM backend; exports the session API (`bootSession`, `evaluate`,
  `advance`, `resolveInput`, `resolveChoice`, `resolveHavoc`, `cancel`,
  `getPrompt`, `parseAst`, `resetSession`, `helpText`, `versionText`) plus
  `start`, a naive stdio REPL for wasmtime & friends
- **`stub.js`**, auto-generated JSFFI bridge (from GHC's `post-link.mjs`)
  converting `JSString`s across the boundary
- **`@runno/wasi`**, in-browser WASI runtime instantiating the module
- **`ghostty-web`**, terminal emulator built on Ghostty's WASM-compiled
  VT100 implementation with an xterm.js-compatible API, plus a local-echo
  addon for line editing, multi-line input, history, and tab-completion
- **Preact + HTM**, app shell with tabs: Terminal, State (live variables,
  procedures, break flag), Trace (statement trace and rule-labelled steps of
  the latest interpretation), AST (parse trees of arbitrary constructs) and
  EBNF (railroad diagrams of the grammar)

Session management lives on the JavaScript side: the WASM module is
stateless across calls. Every API call takes the session as JSON and every
envelope carries the updated session back (`environment`, `flight`, `trace`),
so sessions persist to `localStorage` and resume across page loads. Step
histories stream as per-call deltas in the envelope, and the resource profile
is derived from them client-side.

Evaluation of IMP source returns JSON protocol envelopes: buffered output
events, the step delta, the updated session and a status (`done`, `running`,
`await-input`, `await-havoc`, `await-choice` or `error`). Statements run
under a step budget: each call performs at most 16384 small steps before
pausing with `running`, so the driver yields to the event loop and continues
via `advance` — Ctrl-C lands between chunks and interrupts the program at
any point, at read prompts it cancels the pending resolution (via `cancel`),
and Ctrl-D exits. Suspended interpretation is continued by the JavaScript
driver via the `resolve*` exports — the payload of `await-choice` contains
the focused statement, so the frontend presents a choice screen.

### Build

Building is a two-stage process.

**1. WASM binary** (from the repo root):

```sh
nix build .#impli-web
mkdir -p web/public
cp result/bin/impli.wasm web/public/impli.wasm
cp result/web/stub.js web/public/stub.js
```

**2. Frontend bundle** (from `web/`):

```sh
deno install
deno task build
```

This runs `build.ts`, which generates `examples.js` (a virtual filesystem of
`.imp` files from `docs/examples/`) and bundles the Preact app into
`module.mjs`.

Both stages output to `web/public/`, which is ignored by Git and populated at
build time. The `web/static/` directory contains only committed assets (HTML,
CSS, fonts, favicon) which the deploy workflow copies into `web/public/`
before uploading.

### Deployment

The GitHub Actions workflow (`.github/workflows/deploy.yaml`) runs both stages
on every push to `master`, copies `web/static/` into `web/public/`, and deploys
the contents of `web/public/` to GitHub Pages.
