#![allow(dead_code)] // TODO

#[macro_use]
mod util;

#[macro_use]
extern crate lazy_static;

mod compiler;

fn main() {
	let x = match '9' {
		'0' ... '9' => true,
		_ => false,
	};
	println!("{}", x)
}
