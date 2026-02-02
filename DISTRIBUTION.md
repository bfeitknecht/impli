# Distribution Setup Guide

This document describes the setup required for automated package distribution.

## Prerequisites

### GitHub Secrets

The following secrets need to be configured in the GitHub repository settings:

1. **HACKAGE_TOKEN**: API token for publishing to Hackage
   - Generate at: https://hackage.haskell.org/users/account-management
   - Required for automated Hackage publication

2. **HOMEBREW_TAP_TOKEN**: GitHub personal access token for Homebrew tap
   - Create at: https://github.com/settings/tokens
   - Required permissions: `repo` and `workflow`
   - Required for automated Homebrew tap updates

## Workflow Overview

The `distribute.yaml` workflow is triggered on:
- Push of tags matching `v*` (e.g., `v4.0.0.0`)
- Manual workflow dispatch

The workflow uses GitHub Actions marketplace actions to automate distribution:
- **haskell-actions/hackage-publish** for Hackage
- **Homebrew/actions/homebrew-releaser** for Homebrew
- **jtdor/build-deb-action** for Debian packages

## Distribution Targets

### 1. Hackage

**Automatic**: Yes (requires `HACKAGE_TOKEN` secret)

The workflow automatically publishes to Hackage using the `haskell-actions/hackage-publish` action.

**Manual publication**:
```bash
cabal sdist
cabal upload --publish dist-newstyle/sdist/impli-*.tar.gz
```

### 2. Homebrew

**Automatic**: Yes (requires `HOMEBREW_TAP_TOKEN` secret)

The workflow automatically:
- Creates/updates a Homebrew tap at `bfeitknecht/homebrew-tap`
- Updates the formula with the latest version and checksum
- Pushes changes to the tap repository

**Installation**:
```bash
brew tap bfeitknecht/tap
brew install impli
```

**Manual steps** (for homebrew-core submission):
1. Fork [homebrew-core](https://github.com/Homebrew/homebrew-core)
2. Add the formula from `package/homebrew/impli.rb`
3. Submit a pull request

### 3. Nix/Nixpkgs

**Automatic**: No (requires manual submission)

**Manual steps**:
1. Fork [nixpkgs](https://github.com/NixOS/nixpkgs)
2. Add the package derivation from `package/nixpkgs/default.nix` to `pkgs/by-name/im/impli/package.nix`
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
2. Copy `package/aur/PKGBUILD` to the cloned repository
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

**Automatic**: Yes (builds `.deb` package)

The workflow automatically:
- Builds a Debian package using the `jtdor/build-deb-action`
- Uploads the `.deb` file to GitHub release artifacts

**Installation from release**:
```bash
# Download the .deb file from GitHub releases
sudo dpkg -i impli_*.deb
```

**Manual steps** (for official Debian submission):
1. For official Debian submission, follow the [Debian New Maintainer's Guide](https://www.debian.org/doc/manuals/maint-guide/)
2. For Ubuntu PPA:
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
   - Homebrew tap is updated automatically (if token is configured)
   - Debian package is built and attached to the release
   - AUR PKGBUILD is generated and attached to the release

3. **Manual submissions** (optional):
   - Download the generated AUR PKGBUILD from the GitHub release and publish to AUR
   - Submit formula to homebrew-core for wider distribution
   - Submit package to official Debian/Ubuntu repositories

## Verification

After release, verify:
- [ ] Hackage package is published: https://hackage.haskell.org/package/impli
- [ ] Homebrew tap is updated: https://github.com/bfeitknecht/homebrew-tap
- [ ] Release has distribution artifacts attached (Debian package, AUR PKGBUILD)
- [ ] SHA256 checksums are correct in PKGBUILD

## Troubleshooting

### Hackage publication fails

- Verify `HACKAGE_TOKEN` secret is set correctly
- Check Hackage API status
- Ensure package version doesn't already exist on Hackage

### Homebrew tap update fails

- Verify `HOMEBREW_TAP_TOKEN` secret is set correctly
- Ensure the token has `repo` and `workflow` permissions
- Check that the `bfeitknecht/homebrew-tap` repository exists

### Debian build fails

- Check that all required build dependencies are available
- Verify the `debian/` directory has all required files
- Review GitHub Actions logs for specific build errors

### Workflow fails

- Check GitHub Actions logs
- Verify all required files are present in the repository
- Ensure tag format matches `v*` pattern
