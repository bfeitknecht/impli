## `REPL` Execute Backends

This directory contains the backend-specific `Dispatches` instances for the IMP REPL.

### Modules

| Module | Description |
| --- | --- |
| [`Native`](Native.hs) | Native CLI backend using [haskeline](https://hackage.haskell.org/package/haskeline) for readline-style input, history, and interrupt handling |
| [`Browser`](Browser.hs) | Browser backend compiled to WebAssembly, bridging Haskell and JavaScript via the GHC WASM FFI |

### `Native` Backend

The native backend uses `haskeline`'s `InputT IO` as its base monad. It handles:

- Line input with history and completion via `getInputLine`
- Interrupt (`Ctrl-C`) recovery via `handleInterrupt`
- `Ctrl-D` (EOF) for a clean exit
- ANSI terminal clearing via `ansi-terminal`

### `Browser` Backend

The browser backend uses plain `IO` as its base monad with input routed through a JavaScript FFI stub. It handles:

- Line input via `globalThis.impli` JavaScript callbacks
- Writing trace output to a new browser tab
- Auto-respawn after clean exit (the browser session never terminates)

Both backends share all REPL logic via `REPL.State` and only differ in how they read input, produce output, and dispatch the `Dispatches` typeclass instances.
