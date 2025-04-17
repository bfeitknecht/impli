# `impli`

This is an interpreter for the imperative toy language `IMP` from the course *Formal Methods and Functional Programming* at ETHZ. It is published under the MIT license. Pull requests of any kind are very welcome.


## Usage

To start the REPL, just run `impli` with no arguments. Pass the relative path of an `IMP` source file as argument to interpret it. Directly execute a statement with the `-c` / `--command` option. All variables are initialized to zero. Find some sample programms under `examples/`.


## Installation

Work in progress. Build artifacts for macOS, Linux and Windows *should* be available under the GitHub releases page.


## Limitations

Currently, only the essentials of the specification of `IMP` are supported. One fundamental deviation from it is that an arbitrary nubmer of statements can be sequences withouth the need for parantheses. The following extensions are planned.
- local variable declaration in statement
- procedure declarations and calls
- non-deterministic choice between statements
- parallelism of statements
