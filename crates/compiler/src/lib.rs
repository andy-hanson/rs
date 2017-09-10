#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
#![feature(placement_in_syntax)]

#[macro_use]
extern crate lazy_static;

extern crate host;
extern crate model;
extern crate parse;
#[macro_use]
extern crate util;

mod builtins;
mod check;
mod main;

pub use self::main::{compile, compile_dir, compile_file, full_path, CompileResult, CompiledProgram,
                     EXTENSION};
