## `impli` Web — Frontend Source

This directory contains the TypeScript/Preact source for the `impli` web frontend.

### Files

| File                                 | Description                                                                            |
| ------------------------------------ | -------------------------------------------------------------------------------------- |
| [`App.tsx`](App.tsx)                 | Root Preact component — checks browser compatibility and mounts the terminal           |
| [`impli.ts`](impli.ts)               | `Impli` class — owns the xterm.js terminal, WASM instantiation, and JSFFI callbacks    |
| [`util.ts`](util.ts)                 | Shared utilities: context-aware debug logger and `dedent` template-literal tag         |
| [`Unsupported.tsx`](Unsupported.tsx) | Fallback component displayed when WebAssembly or required browser APIs are unavailable |

### Overview

`App` is the entry point rendered into `document.body`. On mount it instantiates `Impli`, which:

1. Creates an `xterm.js` terminal with `FitAddon` (auto-resize) and `LocalEchoAddon` (line editing, history, tab-completion).
2. Fetches and instantiates `impli.wasm` via `WebAssembly.instantiateStreaming`, wiring up the `@runno/wasi` shim and the auto-generated `stub.js` JSFFI bridge.
3. Calls `exports.start()` to launch the Haskell REPL inside the browser.

The JSFFI bridge exposes `globalThis.impli` so the Haskell side can call back into JavaScript for terminal I/O (`readInput`, `write`, `writeTrace`, `writeWelcome`).

For architecture details and build instructions see the [`web/` README](../README.md).
