first_time:
	# Install from https://rustup.rs
	rustup install nightly
	# Then update_dev_dependencies

clean:
	cargo +nightly clean

check:
	cargo +nightly check

build:
	cargo +nightly build

#  -Zforce-overflow-checks=yes
# -Zperf-stats
# `-- -Zincremental=.incremental` didn't seem to improve anything
run:
	RUST_BACKTRACE=short cargo +nightly run -p main

update_dev_dependencies:
	rustup update
	cargo +nightly install clippy -f
	cargo +nightly install rustfmt-nightly -f

fmt:
	rustup run nightly cargo fmt

lint:
	rustup run nightly cargo clippy
