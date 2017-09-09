// TODO
#![allow(dead_code)]
#![allow(unknown_lints)] // Clippy lints aren't known to rustc stable
//#![feature(core_intrinsics)]
#![feature(custom_attribute)] // Used for serde
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]
#![feature(conservative_impl_trait)]
#![feature(offset_to)]
#![feature(collection_placement)]
#![allow(needless_lifetimes)] //TODO

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

fn main() {
	let exit_code = do_test_single(&Path::of_slice(b"Main-Pass"), /*update_baselines*/ false);
	exit(exit_code)
}
