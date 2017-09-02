pub mod ast;
mod lexer;
mod parse_module;
mod parse_ty;
mod parse_expr;
mod token;

use self::lexer::Lexer;
use self::parse_module::parse_module;

pub use self::lexer::Result;

pub fn parse(source: &[u8]) -> Result<ast::Module> {
	parse_module(&mut Lexer::new(source))
}
