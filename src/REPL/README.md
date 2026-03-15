## `REPL`

This directory contains the Read-Evaluate-Print-Loop (REPL) for the IMP language interpreter.

### Modules

| Module                 | Description                                                                                        |
| ---------------------- | -------------------------------------------------------------------------------------------------- |
| [`Preset`](Preset.hs)  | Default settings (prompt, separator, welcome/goodbye messages, verbosity)                          |
| [`Meta`](Meta.hs)      | Metacommand type definitions and parsers for `Command`, `Aspect`, `Option`, `Level`, and `Element` |
| [`State`](State.hs)    | Shared REPL state (`Store`), the polymorphic `REPL` monad, and all REPL action implementations     |
| [`Execute/`](Execute/) | Backend-specific dispatch implementations for native CLI and browser                               |

### Architecture

The REPL is split into a backend-agnostic core and backend-specific dispatch instances.

```
Input line
    │
    ├─ (:command) ──► [Meta.parses] ──► Command ──► dispatch (Command)
    │
    └─ (program)  ──► [IMP.Parser]  ──► Construct ─► dispatch (Construct)
                                                            │
                                                     ┌──────┴──────┐
                                                     ▼             ▼
                                               Statement      Expression
                                                     │
                                                     ▼
                                              IMP.Statement.execute
                                                     │
                                                     ▼
                                                 Store update
```

### `REPL` Monad

The `REPL` monad is polymorphic in its base monad `m`:

```haskell
type REPL m = StateT Store (ExceptT Exception m)
```

This allows the same core logic (`State.hs`) to be reused across both the native CLI backend (`Execute/Native.hs`, using `InputT IO`) and the browser backend (`Execute/Browser.hs`, using plain `IO`).

### Metacommands

Metacommands are prefixed with `:` and handled separately from IMP source input.
See the `Meta` module for the full list of supported commands and their parsers,
or run `:help` in the REPL for a summary.
