all:
	cargo fmt
	cargo clippy --tests
	cargo test
