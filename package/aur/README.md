# AUR Package

This directory contains the PKGBUILD for the Arch User Repository (AUR).

## Building the package

```bash
cd aur/
makepkg -si
```

## Installation

Install via AUR helper:

```bash
yay -S impli
# or
paru -S impli
```

## Submitting to AUR

To submit this package to AUR:

1. Clone the AUR repository:
   ```bash
   git clone ssh://aur@aur.archlinux.org/impli.git
   ```

2. Copy the PKGBUILD:
   ```bash
   cp PKGBUILD /path/to/aur/repo/
   ```

3. Generate .SRCINFO:
   ```bash
   cd /path/to/aur/repo/
   makepkg --printsrcinfo > .SRCINFO
   ```

4. Commit and push:
   ```bash
   git add PKGBUILD .SRCINFO
   git commit -m "Update to version X.X.X"
   git push
   ```

Note: The SHA256 checksum is automatically updated by the distribute.yaml workflow on tagged releases.
