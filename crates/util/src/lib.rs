#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
#![allow(new_without_default)]
#![allow(new_without_default_derive)]
#![feature(collection_placement)]
#![feature(conservative_impl_trait)]
#![feature(offset_to)]
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]

#[macro_use]
extern crate lazy_static;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate termcolor;

#[macro_use]
pub mod macros;

pub mod arena;
pub mod arith;
pub mod dict;
pub mod file_utils;
pub mod iter;
pub mod path;
pub mod loc;
pub mod late;
pub mod list;
pub mod output_shower;
pub mod show;
pub mod string_maker;
pub mod sync;
pub mod sym;
pub mod up;
pub mod u8_slice_ops;
