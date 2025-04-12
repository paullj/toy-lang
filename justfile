test:
    @cargo llvm-cov nextest

lint:
    @cargo clippy --all-targets --all-features -- -D warnings

format:
    @cargo fmt --all -- --check