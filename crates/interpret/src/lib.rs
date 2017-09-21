#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
#![allow(new_without_default)]
#![feature(collection_placement)]
#![feature(placement_in_syntax)]
#![feature(placement_new_protocol)]

#[macro_use]
extern crate lazy_static;
extern crate serde;
#[macro_use]
extern crate serde_derive;

extern crate model;
#[macro_use]
extern crate util;
extern crate value;

#[macro_use]
mod builtins_macro;
mod builtins;
pub mod emit;
pub mod emitted_model;
mod from_ctx;
pub mod run;
