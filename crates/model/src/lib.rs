#![feature(conservative_impl_trait)]
#![feature(placement_in_syntax)]

extern crate serde;
#[macro_use]
extern crate serde_derive;

extern crate parse_diag;
#[macro_use]
extern crate util;

pub mod builtins;
pub mod class;
pub mod diag;
pub mod document_info;
pub mod effect;
pub mod expr;
pub mod ty;
pub mod method;
pub mod module;
pub mod program;
