// TODO
#![allow(dead_code)]
#![allow(unknown_lints)] // Clippy lints aren't known to rustc stable
#![feature(custom_attribute)] // Used for serde

#[macro_use]
extern crate lazy_static;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

#[macro_use]
mod util;

mod compiler;
mod host;
mod interpreter;
mod model;
mod test;

use test::do_test_single;
use util::path::Path;

fn main() {
	let path = Path::parse(b"Main-Pass");
	do_test_single(&path, /*update_baselines*/ true)
}
