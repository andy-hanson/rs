clean:
	cargo clean

build:
	cargo build

run:
	cargo run

install_deps:
	rustup update
	rustup run nightly cargo install clippy rustfmt-nightly

fmt:
	rustup run nightly cargo fmt -- --write-mode overwrite

lint:
	rustup run nightly cargo clippy
