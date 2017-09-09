first_time:
	# Install from https://rustup.rs
	rustup install nightly
	# Then update_dev_dependencies

clean:
	rustup run nightly cargo clean

check:
	rustup run nightly cargo check

build:
	rustup run nightly cargo build

run:
	RUST_BACKTRACE=short rustup run nightly cargo run

update_dev_dependencies:
	rustup update
	cargo +nightly install clippy -f
	cargo +nightly install rustfmt-nightly -f

fmt:
	rustup run nightly cargo fmt

lint:
	rustup run nightly cargo clippy
