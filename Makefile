first_time:
	# Install from https://rustup.rs
	rustup install nightly
	rustup default nightly # Needed to get IntelliJ to use nightly.
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
	cargo +nightly install clippy rustfmt-nightly -f

fmt:
	cargo +nightly fmt --all

lint:
	cargo +nightly clippy --all
