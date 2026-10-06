## IMP Core

This directory contains the core modules of the IMP language interpreter.

### Modules

| Module                        | Description                                                                                             |
| ----------------------------- | ------------------------------------------------------------------------------------------------------- |
| [`Syntax`](Syntax.hs)         | Abstract syntax definitions for arithmetic expressions, boolean expressions, statements, and procedures |
| [`Lexer`](Lexer.hs)           | Lexical analyzer defining keywords, operators, identifiers, and whitespace parsers                      |
| [`Parser`](Parser.hs)         | Recursive descent parser with operator precedence for all language constructs                           |
| [`Expression`](Expression.hs) | Expression evaluation for arithmetic (`Aexp`), boolean (`Bexp`), and statement (`Stm`) types            |
| [`Statement`](Statement.hs)   | Statement execution dispatcher delegating to the appropriate semantic model                             |
| [`Kernel`](Kernel.hs)         | Pure API-centric stepping layer with explicit suspension/resolution configurations                       |
| [`Engine`](Engine.hs)         | Session runtime layer over `Kernel` with resumable state/history management                              |
| [`State`](State.hs)           | State management, the `IMP` monad, and utility functions                                                |
| [`Pretty`](Pretty.hs)         | Pretty-printing for all language constructs via the `prettyprinter` library                             |
| [`Exception`](Exception.hs)   | Exception type encapsulating parse, assertion, IO, and runtime errors                                   |
| [`Config`](Config.hs)         | Global configuration toggles for semantics and language extensions                                      |

### Architecture

The pipeline follows a standard interpreter architecture:

```
Source text
    │
    ▼
[Lexer] ── tokenizes ──► [Parser] ── builds AST ──► [Statement]
                                                          │
                                           ┌─────────────┴──────────────┐
                                           ▼                             ▼
                                  [Semantics.Natural]        [Semantics.Structural]
                                  (big-step, default)        (small-step, optional)
                                           │                             │
                                           └─────────────┬──────────────┘
                                                         ▼
                                                      [State]
```

The `Config` module controls parser/language toggles (for example `extensions = True`). Statement execution now supports explicit semantic selection (`IMP.Statement.executeWith`) while preserving compatibility with the existing `execute` behavior.
