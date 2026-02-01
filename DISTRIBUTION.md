# Distribution Setup Guide

This document describes the setup required for automated package distribution.

## Prerequisites

### GitHub Secrets

The following secrets need to be configured in the GitHub repository settings:

1. **HACKAGE_USERNAME**: Your Hackage username
   - Your Hackage account username
   - Required for automated Hackage publication

2. **HACKAGE_TOKEN**: API token/password for publishing to Hackage
   - Generate at: https://hackage.haskell.org/users/account-management
   - Required for automated Hackage publication

## Workflow Overview

The `distribute.yaml` workflow is triggered on:
- Push of tags matching `v*` (e.g., `v4.0.0.0`)
- Manual workflow dispatch

## Distribution Targets

### 1. Hackage

**Automatic**: Yes (requires `HACKAGE_TOKEN` secret)

The workflow automatically:
- Builds source distribution with `cabal sdist`
- Publishes to Hackage using `cabal upload`

**Manual publication**:
```bash
cabal sdist
cabal upload --publish dist-newstyle/sdist/impli-*.tar.gz
```

### 2. Homebrew

**Automatic**: Partial (formula generation only)

The workflow automatically:
- Updates SHA256 checksums
- Generates the formula file

**Manual steps**:
1. Fork [homebrew-core](https://github.com/Homebrew/homebrew-core)
2. Add the formula from `homebrew/impli.rb`
3. Submit a pull request

Or create a custom tap:
```bash
brew tap bfeitknecht/tap
# Then add the formula to that tap repository
```

### 3. Nix/Nixpkgs

**Automatic**: No (requires manual submission)

**Manual steps**:
1. Fork [nixpkgs](https://github.com/NixOS/nixpkgs)
2. Add the package derivation from `nixpkgs/default.nix` to `pkgs/by-name/im/impli/package.nix`
3. Submit a pull request following [contribution guidelines](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md)

### 4. AUR (Arch User Repository)

**Automatic**: Partial (PKGBUILD generation only)

The workflow automatically:
- Updates version and SHA256 checksums
- Generates the PKGBUILD file

**Manual steps**:
1. Clone the AUR repository:
   ```bash
   git clone ssh://aur@aur.archlinux.org/impli.git
   ```
2. Copy `aur/PKGBUILD` to the cloned repository
3. Generate `.SRCINFO`:
   ```bash
   makepkg --printsrcinfo > .SRCINFO
   ```
4. Commit and push:
   ```bash
   git add PKGBUILD .SRCINFO
   git commit -m "Update to version X.X.X"
   git push
   ```

### 5. Debian/APT

**Automatic**: Partial (package files generation only)

The workflow automatically:
- Updates the changelog with the new version
- Generates updated Debian package files

**Manual steps**:
1. Build the package:
   ```bash
   dpkg-buildpackage -us -uc -b
   ```
2. For official Debian submission, follow the [Debian New Maintainer's Guide](https://www.debian.org/doc/manuals/maint-guide/)
3. For Ubuntu PPA:
   - Set up Launchpad account
   - Create a PPA
   - Upload the package using `dput`

## Release Process

1. **Tag the release**:
   ```bash
   git tag -a v4.0.0.0 -m "Release version 4.0.0.0"
   git push origin v4.0.0.0
   ```

2. **Wait for workflow completion**:
   - The workflow will automatically run
   - Hackage publication is automatic (if token is configured)
   - Other distribution files are generated and attached to the release

3. **Manual submissions**:
   - Download the generated files from the GitHub release
   - Submit to Homebrew, Nixpkgs, AUR, or Debian as needed

## Verification

After release, verify:
- [ ] Hackage package is published: https://hackage.haskell.org/package/impli
- [ ] Release has distribution artifacts attached
- [ ] SHA256 checksums are correct in Homebrew formula and PKGBUILD

## Troubleshooting

### Hackage publication fails

- Verify `HACKAGE_TOKEN` secret is set correctly
- Check Hackage API status
- Ensure package version doesn't already exist on Hackage

### Workflow fails

- Check GitHub Actions logs
- Verify all required files are present in the repository
- Ensure tag format matches `v*` pattern
