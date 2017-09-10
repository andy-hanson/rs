#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
#![feature(collection_placement)]
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]

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
