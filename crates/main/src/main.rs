// TODO
#![allow(dead_code)]
#![allow(unknown_lints)] // Clippy lints aren't known to rustc stable
#![feature(core_intrinsics)]
#![feature(custom_attribute)] // Used for serde
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]
#![feature(conservative_impl_trait)]

#[macro_use]
extern crate no_drop_derive;
#[macro_use]
extern crate lazy_static;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
//extern crate backtrace;

#[macro_use]
mod util;

mod compiler;
mod host;
mod interpreter;
mod model;
mod test;

use std::process::exit;
use test::do_test_single;
use util::path::Path;

//use std::panic;

fn main() {
	//TODO:KILL
	//panic::set_hook(Box::new(|_| {
	//	println!("!!!!!!!!!!!");
	//	exit(123);
	//}));

	let path = Path::parse(b"Main-Pass");
	let exit_code = do_test_single(&path, /*update_baselines*/ true);
	exit(exit_code)

	//let o: Own<i32> = Own::new(0);
	//let p: Ptr<i32> = o.ptr();
	//unused!(o, p);
}
