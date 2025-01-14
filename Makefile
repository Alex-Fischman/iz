all:
	cargo fmt
	cargo clippy --tests
	cargo test
	cargo run -- scratch.iz
