#![allow(dead_code)]
#![allow(unused_imports)]

extern crate serde_yaml;

extern crate parse;
extern crate test;
extern crate util;

use std::process::exit;

use serde_yaml::to_string as to_yaml_string;

use util::arena::Arena;
use util::output_shower::OutputShower;
use util::path::Path;
use util::show::Shower;
use util::loc::LineAndColumnGetter;

use parse::{parse, ParseDiagnostic};

use test::{BaselinesUpdate, do_test_single};

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
			OutputShower::stdout().add(to_yaml_string(ast).unwrap().as_str())?.nl()?;
		}
		Result::Err(ParseDiagnostic(loc, ref diag)) => {
			let lc = LineAndColumnGetter::new(text);
			OutputShower::stderr().add(&lc.line_and_column_at_loc(loc))?.add(": ")?.add(diag)?.nl()?;
		}
	}
	Ok(())
}
