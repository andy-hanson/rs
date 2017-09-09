#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?

#![feature(placement_in_syntax)]

#[macro_use]
extern crate lazy_static;

extern crate model;
#[macro_use]
extern crate util;

mod builtins;
mod emit;
mod emitted_model;
mod value;
mod run;

pub fn dummy(x: u32) -> u32 {
	x + 1
}
