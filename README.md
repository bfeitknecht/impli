# `impli`

This is an interpreter for the imperative toy language `IMP` from the course *Formal Methods and Functional Programming* at ETHZ. It is published under the MIT license.


## Usage

To start the REPL, just run `impli` with no arguments. Pass the relative path of an `IMP` source file as argument to interpret it. Directly execute a statement with the `--command` option or print its AST with `--ast`. All variables are initialized to zero. Find some sample programs in `examples/`.


## Installation

Work in progress. Build artifacts for macOS, Linux and Windows are available under the GitHub releases page. On macOS, users may need to modify execution privileges and remove the quarantine.
```bash
chmod +x impli-*
xattr -d com.apple.quarantine impli-*
```


## Specification

The most apparent deviations from the specifications are that parentheses are not strictly required for arithmetic operations and relations in expressions. The same applies to sequencing. Additionally, the write-only placeholder variable `_` allows discarding of values. It can only be used on the lefthandside of variable definitions. Furthermore, chained `if` statements may share one single final `end`.
