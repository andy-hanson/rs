clean:
	cargo clean

build:
	cargo build

run:
	cargo run

install_dev_dependencies_first_time:
	rustup run nightly cargo install clippy rustfmt-nightly

update_dev_dependencies:
	rustup update

fmt:
	rustup run nightly cargo fmt -- --write-mode overwrite

lint:
	rustup run nightly cargo clippy
