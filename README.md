# `impli`

This is an interpreter for the imperative toy language `IMP` from the course *Formal Methods and Functional Programming* at ETHZ. It is published under the MIT license.


## Usage

To start the REPL, just run `impli` with no arguments. Pass the relative path of an `IMP` source file as argument to interpret it. Directly execute a statement with the `--command` option or print its AST with `--ast`. All variables are initialized to zero. Find some sample programs under `examples/`.


## Installation

Work in progress. Build artifacts for macOS, Linux and Windows are available under the GitHub releases page.


## Specification

One fundamental deviation from the specification is that an arbitrary number of statements can be sequences without the need for parentheses. Furthermore, in parallel execution the lefthandside dominates race conditions. The EBNF is given below.
https://github.com/bfeitknecht/impli/blob/master/IMP.ebnf
