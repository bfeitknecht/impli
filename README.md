[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/bfeitknecht/impli)

# `impli`

This is an interpreter for the imperative toy language `IMP` from the course _Formal Methods and Functional Programming_ at ETHZ. It is published under the MIT license.

## Usage

To start the REPL, just run `impli` with no arguments. Pass the relative path of an `IMP` source file as argument to interpret it. Directly execute a statement with the `--command` option or print its AST with `--ast`. All variables are initialized to zero. Find some sample programs in `docs/examples/`.

## Installation

Build artifacts for macOS, Linux and Windows are available under the [GitHub releases page](https://github.com/bfeitknecht/impli/releases). You can download the latest release directly using `curl`.

```bash
curl -LO https://github.com/bfeitknecht/impli/releases/download/<TAG>/<BIN>
```

Replace `<TAG>` with the latest release tag and `<BIN>` with the OS appropriate binary name. On macOS, you may need to modify execution privileges and remove the quarantine attribute.

```bash
chmod +x impli-*
xattr -d com.apple.quarantine impli-*
```

## Development

### Using Nix (Recommended)

If you have [Nix](https://nixos.org/) with flakes enabled, you can easily set up a complete development environment:

```bash
# Enter development shell with all tools
nix develop

# Or use direnv to automatically load the environment
echo "use flake" > .envrc
direnv allow
```

The development shell includes:
- GHC 9.4.8 (Haskell compiler)
- cabal-install (build tool)
- fourmolu (code formatter)
- haskell-language-server (IDE support)

Quick commands in the dev shell:
```bash
cabal build        # Build the project
cabal test         # Run tests
cabal run impli    # Run the REPL
fourmolu -i .      # Format code
```

You can also build the project directly with Nix:
```bash
nix build          # Build impli executable to ./result/bin/impli
```

### Using Cabal Directly

If you have GHC and Cabal installed on your system:

```bash
cabal build        # Build the project
cabal run impli    # Run the REPL
cabal test         # Run tests
```

## Specification

The most apparent deviations from the specifications are that parentheses are not required for arithmetic and boolean operations in expressions and sequential composition in statements. Additionally, the write-only placeholder variable `_` allows discarding of values. It can only be used on the lefthandside of variable definitions.

The table below depicts the correspondence between semantics functions defined in the lectures and according functions in this library for `state` $\equiv \sigma$.

| FMFP                                                              | `impli`                                        |
| ----------------------------------------------------------------- | ---------------------------------------------- |
| $\sigma_{\text{zero}}$                                            | `IMP.State.initial`                            |
| $\sigma(x)$                                                       | `IMP.State.getVar state x`                     |
| $\sigma[x \mapsto n]$                                             | `IMP.State.setVar state x n`                   |
| $\langle s, \sigma \rangle \to \sigma'$                           | `IMP.Semantics.Structural.run (s, state)`      |
| $\langle s, \sigma \rangle \underset{1}{\to} \gamma$              | `IMP.Semantics.Operational.step (s, [state])`  |
| $\langle s, \sigma \rangle \overset{*}{\underset{1}{\to}} \gamma$ | `IMP.Semantics.Operational.steps (s, [state])` |
| $\mathcal{N}[[n]]$                                                | `id n`                                         |
| $\mathcal{A}[[a]]\sigma$                                          | `IMP.Expression.evaluate a state`              |
| $\mathcal{B}[[b]]\sigma$                                          | `IMP.Expression.evaluate b state`              |

## Roadmap

There are some crucial points for further improvement.

- ~~Add web-support (probably via compilation to WASM)~~
- Improve the REPL
    - Support tab-autocompletion
    - Handle multi-line input
- Ability to load source modules for interpretation
- Package binary and distribute (Homebrew, APT, Nixpkgs, Hackage ...)
- Improve documentation with examples
- Write standard library
- Extend test suite

## Star History

[![Star History Chart](https://api.star-history.com/svg?repos=bfeitknecht/impli&type=date&legend=top-left)](https://www.star-history.com/#bfeitknecht/impli&type=date&legend=top-left)
