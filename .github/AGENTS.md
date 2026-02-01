# Agents

This document describes the automated agents and tools used in this repository.

## GitHub Actions

The repository uses several GitHub Actions workflows for automation:

- **Test**: Runs the test suite on pull requests and commits
- **Format**: Checks code formatting using fourmolu
- **Deploy**: Deploys the web version of the interpreter
- **Release**: Creates and publishes releases with binaries for multiple platforms

## GitHub Copilot

This repository is configured to work with GitHub Copilot, an AI-powered code completion tool. Contributors are encouraged to use Copilot to assist with:

- Writing Haskell code following the project's conventions
- Creating test cases
- Documenting functions and modules
- Refactoring code

## Bot Automation

The repository may use automated bots for:

- Dependency updates
- Security vulnerability alerts
- Code quality checks
- Release automation

## Contributing with Agents

When contributing to this repository:

1. Ensure that any automated tool usage follows the project's coding standards
2. Review AI-generated code carefully before committing
3. Run local tests before pushing changes
4. Follow the formatting guidelines (see `fourmolu.yaml`)

## Configuration

Agent configurations can be found in:

- `.github/workflows/` - GitHub Actions workflow definitions
- `fourmolu.yaml` - Code formatting rules
- `cabal.project` - Build configuration
