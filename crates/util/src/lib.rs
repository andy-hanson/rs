#![allow(unknown_lints)] // Clippy lints aren't known to rustc?

#![feature(collection_placement)]
#![feature(offset_to)]
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]

#[macro_use]
extern crate lazy_static;
extern crate serde;
#[macro_use]
extern crate serde_derive;

#[macro_use]
pub mod macros;

pub mod arena;
pub mod arith;
pub mod arr;
pub mod dict;
pub mod file_utils;
pub mod path;
pub mod loc;
pub mod late;
pub mod string_maker;
pub mod sync;
pub mod sym;
pub mod utils;
