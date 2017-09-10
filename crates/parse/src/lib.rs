#![allow(dead_code)] // TODO
#![feature(placement_in_syntax)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate util;
extern crate model; //TODO:avoid

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

pub fn parse<'a, 't>(arena: &'a Arena, source: &'t [u8]) -> Result<ast::Module<'a>> {
	parse_module(&mut Lexer::new(arena, source))
}