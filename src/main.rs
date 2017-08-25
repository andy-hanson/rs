#![allow(dead_code)] // TODO

#[macro_use]
mod util;

#[macro_use]
extern crate lazy_static;

mod compiler;

fn half(x: u32) -> Option<u32> {
	if x % 2 == 0 {
		Some(x / 2)
	} else {
		None
	}
}

fn foo(x: u32) -> u32 {
	let h = unwrap_or_return!(half(x), 123);
	h * 2
}

fn main() {
	println!("{}", foo(1));
	println!("{}", foo(2))
}
