## IMP Core

This directory contains the core modules of the IMP language interpreter.

### Modules

| Module                        | Description                                                                                        |
| ----------------------------- | -------------------------------------------------------------------------------------------------- |
| [`Syntax`](Syntax.hs)         | Abstract syntax definitions for arithmetic, boolean expressions, statements, and procedures        |
| [`Lexer`](Lexer.hs)           | Lexical analyzer defining keywords, operators, identifiers, and whitespace parsers                 |
| [`Parser`](Parser.hs)         | Recursive descent parser with operator precedence for all language constructs                      |
| [`Expression`](Expression.hs) | Expression evaluation for arithmetic (`Aexp`) and boolean (`Bexp`) expressions                     |
| [`State`](State.hs)           | State management and pure utility functions                                                        |
| [`Kernel`](Kernel.hs)         | Pure kernel facade: structural `step`/`resume` and natural `run` as an effect program          |
| [`Engine`](Engine.hs)         | Session layer discharging kernel effects through frontend handlers, with trace and profile         |
| [`Pretty`](Pretty.hs)         | Pretty-printing for all language constructs via the `prettyprinter` library                        |
| [`Exception`](Exception.hs)   | Exception type encapsulating parse, assertion, IO, and runtime errors                              |

### Architecture

The interpreter is organized in three layers.

```
Source text
    │
    ▼
[Lexer] ── tokens ──► [Parser] ── AST ──► [Kernel]
                                              │
                              ┌───────────────┴───────────────┐
                              ▼                               ▼
                 [Semantics.Natural]              [Semantics.Structural]
                 big-step, effect program         small-step, pure step/resume
                 over effects                     on configurations
                              └───────────────┬───────────────┘
                                              ▼
                                           [Engine]
                              discharges effects through Handlers
                              supplied by the frontend, manages
                              environment, trace and profile
```

The structural kernel is pure and serialisable: `step :: Gamma -> Configuration`
performs a single transition over `Gamma = (Stm, State)` and may yield a
`Blocked Suspension` for `read`, `havoc` and non-deterministic or parallel
choice, which the frontend resolves and continues with
`resume :: Gamma -> Resolution -> Configuration`. The natural semantics is
written against an abstract effect signature over `Input`, `Output` and `Choose` effects
(fused-effects) plus `Error Exception`. All statements keep the machine
configuration a plain `Gamma`: procedure invocation and local scope rewrite
to `Return`/`Restore` sequences carrying state snapshots, so no state stack
is needed.
