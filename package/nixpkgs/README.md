# Nixpkgs Package

This directory contains the Nix derivation for `impli`.

## Building locally

```bash
nix-build nixpkgs/
```

## Installation

To install `impli` using Nix:

```bash
nix-env -f nixpkgs/ -i
```

## Submitting to nixpkgs

To submit this package to nixpkgs, follow the [nixpkgs contribution guide](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md).
