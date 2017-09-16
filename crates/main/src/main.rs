#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
//#![feature(core_intrinsics)]
#![feature(custom_attribute)] // Used for serde
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]
#![feature(conservative_impl_trait)]
#![feature(offset_to)]
#![feature(core_intrinsics)] // TODO: remove
#![feature(collection_placement)]
#![allow(needless_lifetimes)] //TODO
#![allow(unused_imports)] //TODO

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
use test::{BaselinesUpdate, do_test_single};
use util::path::Path;
use parse::{parse, ParseDiagnostic};
use util::string_maker::{Shower, WriteShower};
use util::arena::Arena;
use util::loc::LineAndColumnGetter;
use serde_json::to_string as to_json_string;

use interpreter::dummy;

fn main() {
	dummy(1);
	//test_parse().unwrap();
	run_test()
}

fn run_test() {
	let exit_code = do_test_single(Path::of_slice(b"Main-Pass"), BaselinesUpdate::Create);
	exit(exit_code)
}


fn test_parse() -> ::std::io::Result<()> {
	let parse_arena = Arena::new();
	let mut x = include_str!("../../../tests/cases/Main-Pass/index.nz").to_owned();
	x.push('\0');
	let text = x.as_bytes();
	let res = parse(&parse_arena, text);
	match res {
		Result::Ok(ref ast) => {
			let mut e = WriteShower::stderr();
			e.add(to_json_string(ast).unwrap().as_str())?;
			e.nl()?;
		}
		Result::Err(ParseDiagnostic(loc, ref diag)) => {
			let lc = LineAndColumnGetter::new(text);
			let mut e = WriteShower::stderr();
			e.add(&lc.line_and_column_at_loc(loc))?.add(": ")?;
			e.add(diag)?;
			e.nl()?;
		}
	}
	Ok(())
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
