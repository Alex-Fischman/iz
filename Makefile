all:
	cargo fmt
	cargo clippy --tests
	cargo test --no-fail-fast
