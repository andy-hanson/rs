#![allow(dead_code)]
#![allow(unused_imports)]

extern crate serde_yaml;

extern crate parse;
extern crate test;
extern crate util;

use std::process::exit;
use test::{BaselinesUpdate, do_test_single};
use util::path::Path;
use parse::{parse, ParseDiagnostic};
use util::show::{Shower, WriteShower};
use util::arena::Arena;
use util::loc::LineAndColumnGetter;
use serde_yaml::to_string as to_yaml_string;

fn main() {
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
			e.add(to_yaml_string(ast).unwrap().as_str())?;
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
