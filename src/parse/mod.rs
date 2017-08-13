pub mod ascii;
pub mod ast;
mod lexer;
mod parse_module;
mod parse_ty;
mod parse_expr;
mod token;

use util::arr::Arr;
use self::lexer::{ Lexer, Result };
use self::parse_module::parse_module;

pub fn parse(source: &Arr<u8>) -> Result<ast::Module> {
	parse_module(&mut Lexer::new(source))
}
