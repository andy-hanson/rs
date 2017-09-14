#![allow(dead_code)] // TODO
#![allow(unknown_lints)]
#![feature(placement_in_syntax)]

#[macro_use]
extern crate lazy_static;
#[allow(useless_attribute)]
#[allow(unused_extern_crates)] // This is used...
extern crate serde;
#[macro_use]
extern crate serde_derive;

extern crate model;
#[macro_use]
extern crate util; //TODO:avoid

use util::arena::Arena;

pub mod ast;
mod reader;
mod lexer;
mod parse_module;
mod parse_ty;
mod parse_expr;
mod token;

use self::lexer::Lexer;
use self::parse_module::parse_module;

pub use self::lexer::Result;

pub fn parse<'ast, 'text: 'ast>(arena: &'ast Arena, source: &'text [u8]) -> Result<ast::Module<'ast>> {
	parse_module(&mut Lexer::new(arena, source))
}
