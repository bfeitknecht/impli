# `impli`

This is an interpreter for the imperative toy language `IMP` from the course *Formal Methods and Functional Programming* at ETHZ. It is published under the MIT license.


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


## Specification

The most apparent deviations from the specifications are that parentheses are not strictly required for arithmetic operations and relations in expressions. The same applies to sequencing. Additionally, the write-only placeholder variable `_` allows discarding of values. It can only be used on the lefthandside of variable definitions.


## Roadmap

There are some crucial points of further improvement. These include the following.
- improve the REPL
    - cleanly handle interrupt (ctrl+c)
    - support tab-autocompletion
    - handle multi line input
- ability to load source modules for interpretation
- distribute package (homebrew, AUR, nixpkgs, hackage ..)
- improve documentation with examples
- write standard library
- extend test suite
