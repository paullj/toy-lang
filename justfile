set dotenv-load

_default:
  @just --list

# Run tests
test:
    @cargo llvm-cov nextest

# Run linter
lint:
    @cargo clippy --all-targets --all-features
    # @cargo clippy --all-targets --all-features -- -D warnings

# Run formatter
format:
    @cargo fmt --all -- --check

# Watches for changes and runs linter, formatter, and tests
watch:
    @bacon test