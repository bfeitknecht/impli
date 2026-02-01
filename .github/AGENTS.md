# AGENTS Guidelines for This Repository

This repository contains `impli`, an interpreter for the imperative toy language `IMP` written in Haskell. When working on this project with AI coding agents (e.g., GitHub Copilot, Claude, etc.), please follow the guidelines below to ensure a smooth development experience.

## Project Overview

**impli** is an educational Haskell project that implements an interpreter for IMP, a minimal imperative language used in the _Formal Methods and Functional Programming_ course at ETHZ. The project includes:

- A library implementing the IMP language semantics
- A CLI executable with REPL (Read-Eval-Print Loop)
- A web-based WASM version deployable to GitHub Pages
- Comprehensive test suite
- Cross-platform releases (macOS, Linux, Windows)

## High-Level Architecture

```
impli/
├── src/                    # Core library implementation
│   └── IMP/
│       ├── Syntax.hs       # AST definitions (Aexp, Bexp, Stm)
│       ├── Lexer.hs        # Tokenization
│       ├── Parser.hs       # Parsing (using Parsec)
│       ├── Expression.hs   # Expression evaluation
│       ├── Statement.hs    # Statement execution
│       ├── State.hs        # Interpreter state management
│       ├── Semantics/
│       │   ├── Structural.hs  # Big-step semantics
│       │   └── Operational.hs # Small-step semantics
│       ├── Pretty.hs       # Pretty printing
│       ├── Config.hs       # Configuration
│       └── Exception.hs    # Error handling
├── exe/                    # Executable implementations
│   ├── Main.hs            # CLI entry point
│   ├── CLI.hs             # Command-line interface
│   ├── Web.hs             # WASM entry point
│   └── REPL/              # REPL implementation
├── test/                   # Test suite
├── docs/                   # Documentation & examples
├── web/                    # Web frontend assets
└── .github/workflows/      # CI/CD automation
```

## Detailed Module Structure

### Core Library (`src/IMP/`)

- **Syntax.hs**: Defines the abstract syntax tree (AST)
  - `Aexp` - Arithmetic expressions
  - `Bexp` - Boolean expressions  
  - `Stm` - Statements (skip, assignment, sequence, if/then/else, while, etc.)
  - `Proc` - Procedure definitions
  
- **Lexer.hs & Parser.hs**: Transform source code into AST using Parsec combinator library

- **Expression.hs**: Evaluates arithmetic and boolean expressions in a given state

- **Statement.hs**: Executes statements, modifying interpreter state

- **State.hs**: Manages interpreter state as `(Vars, [Proc], Bool)` tuple:
  - `Vars` - Map of variables to integer values
  - `[Proc]` - List of defined procedures
  - `Bool` - Break flag for loop control

- **Semantics/**: Two semantic implementations
  - **Structural.hs**: Big-step operational semantics (`⟨s, σ⟩ → σ'`)
  - **Operational.hs**: Small-step operational semantics with configurations

### Executables (`exe/`)

- **Main.hs**: CLI entry point handling file interpretation, REPL mode, and command execution
- **CLI.hs**: Command-line argument parsing using optparse-applicative
- **Web.hs**: WASM-compatible entry point (no threading, compatible with wasm32-wasi)
- **REPL/**: Interactive REPL implementation with state management

## Development Environment Setup

### Prerequisites

- GHC 9.10.2 or 9.12.2 (Haskell compiler)
- Cabal 3.12 or 3.14 (build tool)
- fourmolu 0.16.2.0 (code formatter)
- Optional: Nix (for WASM builds)

### Building the Project

```bash
# Update Cabal package list
cabal update

# Build the library and CLI executable
cabal build impli

# Build the WASM version (requires Nix)
nix build .#impli-web

# Build test suite
cabal build impli-test
```

### Running Tests

```bash
# Run all tests
cabal test impli-test

# Run specific test file
cabal test impli-test --test-show-details=direct
```

### Code Formatting

**ALWAYS** format Haskell code using fourmolu before committing:

```bash
# Check formatting (use this in CI)
fourmolu --mode check $(git ls-files '*.hs')

# Auto-format all Haskell files
fourmolu --mode inplace $(git ls-files '*.hs')
```

Configuration is defined in `fourmolu.yaml`. Some modules have `{- FOURMOLU_DISABLE -}` pragma at the top to preserve manual formatting.

### Running the Interpreter

```bash
# Start REPL
cabal run impli

# Interpret a file
cabal run impli -- path/to/file.imp

# Execute a direct command
cabal run impli -- --command "x := 5; print x"

# Print AST
cabal run impli -- --ast "x := 5"
```

## Coding Conventions

### Haskell Style Guidelines

1. **Module Documentation**: Every module should have a header with:
   - Module name
   - Description
   - Copyright
   - License (MIT)
   - Maintainer email
   - Stability and portability

2. **Function Documentation**: Use Haddock comments (`-- |`) for all exported functions

3. **Language Extensions**: Enable pragmas at the top when needed (e.g., `FlexibleContexts`)

4. **Imports**: 
   - Group standard library imports first
   - Then qualified imports
   - Finally local module imports
   - Use explicit import lists or qualified imports to avoid namespace pollution

5. **Naming Conventions**:
   - Types: `PascalCase` (e.g., `Aexp`, `State`)
   - Functions: `camelCase` (e.g., `evaluate`, `getVar`)
   - Constants: `camelCase` (e.g., `initial`, `zero`)

6. **Type Signatures**: Always provide explicit type signatures for top-level functions

### IMP Language Conventions

When extending the language or modifying semantics:

1. Consult `IMP.ebnf` for the grammar specification
2. Update the EBNF if adding new constructs
3. Maintain correspondence with formal semantics from FMFP course
4. Special variable `_` is write-only and used for discarding values

## Testing Instructions

### Test Organization

Tests are located in `test/Spec.hs` using the Tasty testing framework with HUnit assertions.

### Writing Tests

When adding new features:

1. Add test cases to `test/Spec.hs`
2. Test both structural and operational semantics when applicable
3. Include edge cases (e.g., division by zero, undefined variables)
4. Test error handling and exception cases

### CI Testing

The test workflow (`.github/workflows/test.yaml`) runs on:
- Every push to `master`
- All pull requests
- Manual workflow dispatch

Tests use GHC 9.12.2 and Cabal 3.14 in CI.

## GitHub Actions Workflows

### Test (`.github/workflows/test.yaml`)
- **Trigger**: Push to master, PRs, manual
- **Purpose**: Run test suite
- **Environment**: Ubuntu with GHC 9.12.2

### Format (`.github/workflows/format.yaml`)
- **Trigger**: Push to master, PRs, manual
- **Purpose**: Verify code formatting with fourmolu
- **Environment**: Ubuntu with GHC 9.10.2
- **Fails if**: Any `.hs` file is not properly formatted

### Deploy (`.github/workflows/deploy.yaml`)
- **Trigger**: Push to master, PRs, manual
- **Purpose**: Build WASM version and deploy to GitHub Pages
- **Steps**:
  1. Build `impli-web` with Nix WASM backend
  2. Copy WASM binary to `web/static/`
  3. Copy example `.imp` files to `web/static/`
  4. Deploy to GitHub Pages (master branch only)

### Release (`.github/workflows/release.yaml`)
- **Trigger**: Version tags (`v*`), manual
- **Purpose**: Build and release binaries for all platforms
- **Platforms**: Linux (x86_64), macOS (x86_64/aarch64), Windows (x86_64)
- **Artifacts**: Platform-specific binaries (e.g., `impli-x86_64-linux`)

## Common Development Tasks

### Adding a New Statement Type

1. Add constructor to `Stm` in `src/IMP/Syntax.hs`
2. Update parser in `src/IMP/Parser.hs` to recognize the syntax
3. Add lexer tokens if needed in `src/IMP/Lexer.hs`
4. Implement semantics in `src/IMP/Semantics/Structural.hs`
5. Optionally add small-step semantics in `src/IMP/Semantics/Operational.hs`
6. Update pretty printer in `src/IMP/Pretty.hs`
7. Update `IMP.ebnf` grammar
8. Add test cases
9. Format code with fourmolu
10. Run tests before committing

### Adding a New Operator

1. Define operator type in `src/IMP/Syntax.hs` (e.g., `Aop`, `Rop`)
2. Add to parser in `src/IMP/Parser.hs`
3. Implement evaluation in `src/IMP/Expression.hs`
4. Update pretty printer
5. Add tests

### Working with the REPL

REPL code is in `exe/REPL/`:
- `State.hs` - REPL state management
- `Execute.hs` - Statement execution in REPL context
- `Meta.hs` - Meta-commands (`:help`, `:quit`, etc.)
- `Preset.hs` - Predefined examples
- `Web.hs` - Web-specific REPL handling

## Pull Request Guidelines

### Before Submitting a PR

1. **Format code**: `fourmolu --mode inplace $(git ls-files '*.hs')`
2. **Run tests**: `cabal test impli-test`
3. **Verify build**: `cabal build impli`
4. **Check formatting passes CI**: `fourmolu --mode check $(git ls-files '*.hs')`
5. **Update documentation** if adding features
6. **Add examples** in `docs/examples/` for new language features

### PR Title Format

Use descriptive titles:
- `feat: Add support for X statement`
- `fix: Resolve parser issue with Y`
- `docs: Update README with Z`
- `refactor: Simplify W implementation`
- `test: Add test cases for V`

### Review Process

PRs automatically trigger:
- Test suite execution
- Formatting verification
- WASM build (if applicable)

Ensure all checks pass before requesting review.

## Dependency Management

### Adding Dependencies

1. Add to appropriate section in `impli.cabal`:
   - `common shared` - Core library dependencies
   - `common runnable` - CLI executable dependencies
   - Test suite has its own `build-depends`

2. Specify version bounds: `package >=min && <max`

3. Update after adding: `cabal update && cabal build`

### Current Key Dependencies

- **parsec**: Parser combinator library
- **prettyprinter**: Pretty printing
- **mtl**: Monad transformers (ExceptT for error handling)
- **containers**: Data structures (Map for state)
- **haskeline**: REPL line editing
- **optparse-applicative**: CLI argument parsing
- **tasty & tasty-hunit**: Testing framework

## Troubleshooting

### Build Issues

- **Dependency conflicts**: Run `cabal update` then `cabal clean && cabal build`
- **GHC version mismatch**: Check `.github/workflows/` for required GHC version
- **WASM build fails**: Ensure Nix is properly installed with flakes enabled

### Test Failures

- Run individual tests: Modify `test/Spec.hs` to focus on specific test
- Check for state pollution between tests
- Verify examples in `docs/examples/` still work

### Formatting Issues

- **fourmolu not found**: `cabal install fourmolu-0.16.2.0`
- **Formatting check fails**: Run `fourmolu --mode inplace $(git ls-files '*.hs')`
- **Want to preserve formatting**: Add `{- FOURMOLU_DISABLE -}` at module top

## Resources

- [IMP Grammar Specification](../IMP.ebnf)
- [Example Programs](../docs/examples/)
- [GitHub Releases](https://github.com/bfeitknecht/impli/releases)
- [Live Web Version](https://bfeitknecht.github.io/impli/)
- [FMFP Course Materials](https://www.vorlesungen.ethz.ch/)

## Tips for AI Agents

1. **Always check `IMP.ebnf`** before modifying parser or syntax
2. **Maintain semantic correspondence** with formal definitions in comments
3. **Use existing patterns**: Follow established code structure in similar modules
4. **Test both semantics**: Verify changes work in both structural and operational semantics when applicable
5. **Respect fourmolu**: Format code before committing, or add disable pragma if needed
6. **Consider WASM limitations**: Remember `impli-web` doesn't support threading (`-threaded` flag)
7. **Update examples**: Add example programs when introducing new language features
8. **Check cross-platform**: Consider implications for Linux, macOS, and Windows builds
