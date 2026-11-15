## IMP Core

This directory contains the core modules of the IMP language interpreter.

### Modules

| Module                        | Description                                                                         |
| ----------------------------- | ----------------------------------------------------------------------------------- |
| [`Syntax`](Syntax.hs)         | Abstract syntax of arithmetic and boolean expressions, statements and procedures    |
| [`Lexer`](Lexer.hs)           | Lexical analyzer defining keywords, operators, identifiers and whitespace parsers   |
| [`Parser`](Parser.hs)         | Parser with operator precedence for all language constructs                         |
| [`Pretty`](Pretty.hs)         | Pretty-printing with precedence-aware parentheses, round-trips through the parser   |
| [`State`](State.hs)           | State of variables and procedures, and the control flow outcome `Flow`              |
| [`Expression`](Expression.hs) | Evaluation of arithmetic and boolean expressions                                    |
| [`Check`](Check.hs)           | Static checks before execution, e.g. `break` outside loop                           |
| [`Effect`](Effect.hs)         | `MonadIMP`, the interface through which the semantics reach the outside world       |
| [`Interpret`](Interpret.hs)   | Entrypoint: check and execute a statement with either semantics                     |
| [`Exception`](Exception.hs)   | Exception type for parse, assertion, IO and runtime errors                          |

### Architecture

```
Source ──► [Parser] ── Stm ──► [Check] ──► [Interpret]
                                              │
                         ┌────────────────────┴────────────────────┐
                         ▼                                         ▼
               [Semantics.Natural]                      [Semantics.Structural]
               big-step, over MonadIMP                  pure step on (Term, State),
                                                        driven over MonadIMP
```

Frontends (`app/Frontend`) provide a `MonadIMP` instance and use the shared
REPL logic in `src/REPL`. Planned tools build on the same pieces: the stepper
and state debugger on `Structural.step`, which exposes every transition with
its rule and residual configuration, proof trees on a recording instance of
the natural semantics, the AST view on `Parser`.
