clean:
	cargo clean

build:
	cargo build

run:
	cargo run

update_dev_dependencies:
	rustup update
	cargo +nightly install clippy -f
	cargo +nightly install rustfmt-nightly -f

fmt:
	rustup run nightly cargo fmt

lint:
	rustup run nightly cargo clippy
