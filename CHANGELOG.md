# Changelog

All notable changes to this project will be documented in this file.

---

## [6.0.0] - 2026-11-10

The next (hopefully last) huge refactor (#80, also #79, #77, #31).

- Restructure interpreter into a pure kernel, an engine, and thin frontends
  - `IMP.Semantics.Structural`: pure, serialisable small-step semantics
    on configurations with explicit suspension/resolution for `read`,
    `havoc` and non-deterministic or parallel choice; complete coverage
    of all statements of the language
  - `IMP.Semantics.Natural`: big-step semantics against an abstract
    effect signature (fused-effects) over `Input`/`Output`/`Choose`
    effects and `Error`, interpreted by the engine
  - Stack-free machine: procedure invocation and local scope rewrite to
    `Return`/`Restore` sequences carrying state snapshots, the machine
    configuration stays a plain `(Stm, State)`
  - `IMP.Engine`: session layer discharging effects through frontend
    handlers, managing environment, statement trace, step history and
    resource profile
- Remove `IMP.Config` and `IMP.Statement` and the old REPL architecture
  (`REPL.*`, `IMP.Execute.*`); language extensions are now always enabled
- Native frontend improvements
  - Drop VIM input mode and customizable welcome/prompt/separator/goodbye
  - Verbosity levels `normal`/`profile`/`debug` via `--verbose` CLI flag
    and `:set verbose LEVEL` REPL command; level switches prompt
    separator (`>`, `+`, `*`); `profile` reports elapsed time and steps
    taken, `debug` additionally prints the parsed AST and intermediate
    small-step configurations
  - `--structural` CLI flag to interpret with the small-step semantics
  - Multi-line input: lines leaving parentheses or blocks unbalanced
    continue on subsequent lines
- Web frontend rework
  - Terminal emulator switched from `xterm.js` to `ghostty-web`: Ghostty's
    WASM-compiled VT100 implementation behind an xterm.js-compatible API
  - Export-only WASM API; no foreign imports, no side-channel globals
  - JSON (aeson) protocol envelopes: buffered output events plus status
    (`done`, `await-input`, `await-havoc`, `await-choice`, or `error`)
  - JavaScript drives prompt, input and all metacommands; suspended
    interpretation resolves interactively (input prompt, havoc, and a
    choice screen displaying both branches)
  - Ctrl-C interrupts, Ctrl-D exits; multi-line input via local-echo
  - App shell with tabs: Terminal, State, Trace (inference rules), AST,
    and EBNF railroad diagrams

---

## [5.0.0] - 2026-02-25

- Complete web support with WASM build and `xterm.js` integration
  - Map example files into WASI FS for the web REPL
  - Add automatic theme support for web REPL
  - Add filename and meta command completion, link to repository
- Reorganize project structure (move `exe` to `src`)
- Correct semantics modules' names (`Operational` to `Structural`, `Structural` to `Natural`)
- Change constructors of `ProcDef`, `ProcInvoc`
- Remove superfluous `Procedure` wrapper
- Use `Map.Map` instead of list to store procedures in the state
- Change syntax: `var` to `let`, `repeat` to `do`, `do ... times` to `repeat ... times`
- Refactor REPL structure: create `REPL.State` and `REPL.Web` modules
- Update workflows to use GHC 9.12.2 and add ARM64 Windows and aarch64-linux build support
- Add whitepaper

---

## [4.0.0] - 2026-01-13

- Change argument order in `evaluate` to `a -> State -> b` to align with theoretical counterpart
- Start implementation of web support
- Major restructure of the `REPL` module to use `State` monad
- Implement `:set` and `:unset` meta commands
- General code cleanup and linting

---

## [3.0.0] - 2025-08-30

- Separated `REPL` monad stack into `IMP` for interpreter logic and `REPL` for the REPL
- Complete rewrite of the REPL for better user experience
- Major restructure of the `Semantic/` module
- Updated manpages and documentation

---

## [2.1.1] - 2025-07-28

- Renamed some types and constructors for clarity
- Updated `IMPM.ebnf`
- Improved cosmetics throughout the codebase
- Simplified `main` function
- General maintenance and code cleanup

---

## [2.1.0] - 2025-07-16

- Add `alternate` operator.
- Fix procedure invocation not setting return values in `step`.
- Check mismatch of procedure signatures.
- Rename `execute` to `run` to align with movement metaphor.
- Add `interpret` function as API for `steps` or `run`.
- Fix bug of `:reset` without argument not being recognized.
- Clean up `:ast` logic in `handleMeta`.
- Use `unlines` to remove some `++ "\n"` shenanigans.
- Extend partial implementation of typeclasses for `Aexp`.
- Small changes to documentation and UX.

---

## [2.0.2] - 2025-06-05

- Implement statement type `Timeout Stm Aexp`.
- Remove internal variables from output of `:state` in REPL.
- Fulfill invariant that variable names are not the empty string.
- Make `Aexp` partial instance of `Num` to support arithmetic of abstract syntax.
- Move `examples/` and `man/` into `docs/`.

---

## [2.0.1] - 2025-05-16

- Introduce configuration module.
- Implement small step semantics.
- Improve Haddock documentation coverage across modules.
- Add `extra-doc-files` and `extra-source-files` in `.cabal` file.

---

## [2.0.0] - 2025-05-15

- Support for division and modulo operations.
- Language extensions:
  - `match` statement.
  - `havoc` statement.
  - `assert` statement.
  - `flip` statement.
  - `raise` statement.
  - `try` statement.
  - `swap` statement.
- Replace `||` with `[]` for non-determinism.
- Improved REPL and CLI.
- Enhanced pretty printing.
- Cleaner result handling.

---

## [1.1.1] - 2025-05-07

- Delete `TTY` module.

---

## [1.1.0] - 2025-05-07

- Implement `read`, `revert`, and `break` statements.
- Store procedures in state as a list.
- Extend test suite.

---

## [1.0.5] - 2025-05-03

- Fix Windows build workflow.

---

## [1.0.4] - 2025-05-03

- Implement `do-times` statement.
- Move `TTY` module out of `IMP`.
- Strip binary in release workflow.
- Extend tests.

---

## [1.0.3] - 2025-05-01

- Various small fixes.

---

## [1.0.2] - 2025-05-01

- Implement `repeat-until` and `for` statements.
- Implement `time` arithmetic expression.
- Support arithmetic variable definition operators.
- Remove quotes around pretty-printing of `Bool`.
- Rename `Var` to `Ident`.
- Update EBNF.

---

## [1.0.1] - 2025-04-30

- Rewrite parser in applicative style.
- Add specification tests.

---

## [1.0.0] - 2025-04-24

- Core language features and extensions fully implemented.
- Fletched out CLI and REPL.
- Add example programs.
- Pretty printing of statements.

---

## [0.2.0] - 2025-04-19

- Fix procedures.
- Print expressions in the REPL.
- More meta commands and example programs.

---

## [0.1.4] - 2025-04-19

- Fix evaluation of expressions in the REPL.
- More meta commands.
- Internal refactor into `CLI` module.
- Add `version` flag.

---

## [0.1.3] - 2025-04-18

- Add local variable definition in statements.
- Add parallel execution of statements.
- Add non-deterministic execution of statements.
- Known issue: entering only expressions hangs the REPL.

---

## [0.1.2] - 2025-04-17

- Fix while loop logic with eager evaluation.
- Support REPL meta commands.
- Reserve future operators and keywords.

---

## [0.1.1] - 2025-04-16

- Add input channel in `parseIMP`.
- Rename `--code` to `--command`.
- Minor codebase and CI changes for automatic releases.

---

## [0.1.0] - 2025-04-16

- Initial release.
- First release of the IMP CLI tool.
- Contains the interpreter, REPL, and some example programs.

---
