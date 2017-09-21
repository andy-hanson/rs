#![allow(unknown_lints)]
#![allow(new_without_default_derive)]
#![allow(unneeded_field_pattern)]
#![feature(placement_in_syntax)]

extern crate model;
extern crate util;

mod ctx;
mod heap;
mod shape;
mod stack;
mod value;

pub use ctx::ValueCtx;
pub use value::Value;
