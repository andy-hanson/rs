#![allow(dead_code)] // TODO
#![allow(unknown_lints)]
#![feature(placement_in_syntax)]
#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;

extern crate ast;
extern crate parse_diag;
extern crate util;

use util::arena::Arena;

mod reader;
mod lexer;
mod parse_module;
mod parse_ty;
mod parse_expr;
mod token;

use self::lexer::Lexer;
use self::parse_module::parse_module;

pub use self::lexer::{ParseDiagnostic, Result};

pub fn parse<'ast, 'text: 'ast>(arena: &'ast Arena, source: &'text [u8]) -> Result<ast::Module<'ast>> {
	parse_module(&mut Lexer::new(arena, source))
}
