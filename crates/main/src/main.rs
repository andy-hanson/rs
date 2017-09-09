#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
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
extern crate serde_json;

extern crate compiler;
extern crate host;
extern crate interpreter;
extern crate model;
#[macro_use]
extern crate util;

mod test;

use std::process::exit;
use test::do_test_single;
use util::path::Path;

use interpreter::dummy;

fn main() {
	dummy(1);
	let exit_code = do_test_single(&Path::of_slice(b"Main-Pass"), /*update_baselines*/ false);
	exit(exit_code)
}
