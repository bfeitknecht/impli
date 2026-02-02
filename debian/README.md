# Debian Package

This directory contains Debian packaging files for `impli`.

## Building the package

From the repository root:

```bash
dpkg-buildpackage -us -uc -b
```

## Installation

After building:

```bash
sudo dpkg -i ../impli_*.deb
```

## Submitting to Debian

To submit this package to Debian, follow the [Debian packaging guidelines](https://www.debian.org/doc/manuals/maint-guide/).
