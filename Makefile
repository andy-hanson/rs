first_time:
	# Install from https://rustup.rs
	rustup install nightly
	rustup default nightly # Needed to get IntelliJ IDEA to use nightly.
	# Then `make update`

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

update:
	rustup update
	cargo +nightly install clippy rustfmt-nightly -f

fmt:
	cargo +nightly fmt --all

lint:
	cargo +nightly clippy --all
