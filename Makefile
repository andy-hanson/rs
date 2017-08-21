build:
	cargo build

run:
	cargo run

install_deps:
	rustup update
	rustup run nightly cargo install clippy

lint:
	rustup run nightly cargo clippy
