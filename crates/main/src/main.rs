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
extern crate parse;
#[macro_use]
extern crate util;

mod test;

use std::process::exit;
use test::do_test_single;
use util::path::Path;

use interpreter::dummy;

fn main() {
	dummy(1);
	let exit_code = do_test_single(Path::of_slice(b"Main-Pass"), /*update_baselines*/ false);
	exit(exit_code)
}

/*
	//use util::loc::{LineAndColumnGetter, Pos, LineAndColumn};
	//use util::arith::{u32_to_usize, usize_to_u32};

	let sample_text = include_str!("../../../builtins/Bool.nz").as_bytes();
	let lcg = LineAndColumnGetter::new(sample_text); //[0,8,9,33]

	for (i, _) in sample_text.iter().enumerate() {
		let index = usize_to_u32(i);
		let p1 = lcg.line_and_column_at_pos(Pos { index });
		let p2 = get_line_column_slow(sample_text, index);
		if p1 != p2 {
			panic!("{}", i)
		}
	}

	fn get_line_column_slow(text: &[u8], pos: u32) -> LineAndColumn {
		let mut line = 0;
		let mut column = 0;
		for ch in text.iter().take(u32_to_usize(pos)) {
			match *ch {
				b'\n' => {
					line += 1;
					column = 0;
				}
				_ => {
					column += 1;
				}
			}
		}
		LineAndColumn { line, column }
	}
*/
