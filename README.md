[![Release Binaries](https://github.com/bfeitknecht/impli/actions/workflows/release.yaml/badge.svg)](https://github.com/bfeitknecht/impli/actions/workflows/release.yaml)
[![Deploy GitHub Pages](https://github.com/bfeitknecht/impli/actions/workflows/deploy.yaml/badge.svg)](https://github.com/bfeitknecht/impli/actions/workflows/deploy.yaml)
[![Test Specifications](https://github.com/bfeitknecht/impli/actions/workflows/test.yaml/badge.svg)](https://github.com/bfeitknecht/impli/actions/workflows/test.yaml)
[![Test Formatting](https://github.com/bfeitknecht/impli/actions/workflows/format.yaml/badge.svg)](https://github.com/bfeitknecht/impli/actions/workflows/format.yaml)
[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/bfeitknecht/impli)

# `impli`

This is an interpreter for the imperative toy language `IMP` from the course _Formal Methods and Functional Programming_ at ETH Zürich. It is published under the MIT license.

## Usage

To start the REPL, just run `impli` with no arguments. Pass the path of an `IMP` source file as argument to interpret it. Directly execute a statement with the `--command` option or print its AST with `--ast`. In the initial state all variables are uninitialized with zero and no procedure is defined. Find some sample programs in `docs/examples/`.

## Installation

Build artifacts for macOS, Linux, and Windows are available under [GitHub Releases](https://github.com/bfeitknecht/impli/releases). You can download the latest version directly using `curl`. Replace `<BIN>` with the OS appropriate binary name found on the releases page, for example `impli-aarch64-darwin` or `impli-x86_64-windows.exe`.

```bash
 curl -L "https://github.com/bfeitknecht/impli/releases/latest/download/<BIN>" -o impli
```

You may need to modify execution privileges and remove the quarantine attribute on macOS.

```bash
chmod +x impli
xattr -d com.apple.quarantine impli
```

## Specification

The most apparent deviations from the specifications are that parentheses are not required for arithmetic or boolean operations in expressions, and sequential composition in statements. Additionally, the write-only placeholder variable `_` allows discarding of values. It can only be used on the lefthandside of variable definitions. For complete specification of the syntax refer to the [EBNF](docs/IMP.ebnf).

The table below depicts the correspondence between semantics functions defined in the lectures and according functions in this project for $\sigma \equiv$ `state`. For an overview of all natural semantics inference rules, check out the [paper](docs/paper/IMP.pdf).

| FMFP                                                              | `impli`                                       |
| ----------------------------------------------------------------- | --------------------------------------------- |
| $\sigma_{\text{zero}}$                                            | `IMP.State.initial`                           |
| $\sigma(x)$                                                       | `IMP.State.getVar state x`                    |
| $\sigma[x \mapsto n]$                                             | `IMP.State.setVar state x n`                  |
| $\langle s, \sigma \rangle \to \sigma'$                           | `IMP.Semantics.Natural.run (s, state)`        |
| $\langle s, \sigma \rangle \underset{1}{\to} \gamma$              | `IMP.Semantics.Structural.step (s, [state])`  |
| $\langle s, \sigma \rangle \overset{*}{\underset{1}{\to}} \gamma$ | `IMP.Semantics.Structural.steps (s, [state])` |
| $\mathcal{N}[[n]]$                                                | `n`                                           |
| $\mathcal{A}[[a]]\sigma$                                          | `IMP.Expression.evaluate a state`             |
| $\mathcal{B}[[b]]\sigma$                                          | `IMP.Expression.evaluate b state`             |

## Star History

[![Star History Chart](https://api.star-history.com/svg?repos=bfeitknecht/impli&type=date&legend=top-left)](https://www.star-history.com/#bfeitknecht/impli&type=date&legend=top-left)
