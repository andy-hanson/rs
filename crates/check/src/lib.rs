#![allow(dead_code)] // TODO
#![feature(collection_placement)]
#![feature(placement_in_syntax)]

extern crate ast;
extern crate model;
#[macro_use]
extern crate util;

mod ast_utils;
mod check_class;
mod check_expr;
mod class_utils;
mod ctx;
pub mod expected; //TODO: not pub?
mod instantiator;
mod ty_utils;

pub use self::check_class::check_module;
