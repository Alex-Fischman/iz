all:
	cargo fmt
	cargo clippy --tests
	cargo run -- scratch.iz
