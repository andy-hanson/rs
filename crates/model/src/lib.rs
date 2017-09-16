#![feature(placement_in_syntax)]

extern crate serde;
#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate util;
extern crate parse_diag;

pub mod builtins;
pub mod class;
pub mod diag;
pub mod document_info;
pub mod effect;
pub mod expr;
pub mod ty;
pub mod method;
pub mod module;
