#![allow(dead_code)] // TODO
#![allow(unknown_lints)]
#![allow(unneeded_field_pattern)]
#![feature(collection_placement)]
#![feature(conservative_impl_trait)]
#![feature(placement_in_syntax)]
#![feature(try_trait)]

extern crate ast;
extern crate model;
extern crate util;

mod ast_utils;
mod check_class;
mod check_expr;
mod class_utils;
mod ctx;
mod expected;
mod inferrer;
mod instantiator;
mod ty_utils;

pub use self::check_class::check_module;
