use util::list::List;
use util::loc::Pos;
use util::sym::Sym;

use model::effect::Effect;

use super::ast;
use super::lexer::{Lexer, Result};
use super::token::Token;

pub enum SelfEffectOrTy<'a> {
	SelfEffect(Effect),
	Ty(ast::Ty<'a>),
}
pub fn parse_self_effect_or_ty<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<SelfEffectOrTy<'a>> {
	let start = l.pos();
	let token = l.next_token();
	match token {
		Token::Get | Token::Set | Token::Io => {
			l.take_space()?;
			let effect = match token {
				Token::Get => Effect::Get,
				Token::Set => Effect::Set,
				Token::Io => Effect::Io,
				_ => panic!(),
			};
			match l.next_token() {
				Token::SelfKw => Ok(SelfEffectOrTy::SelfEffect(effect)),
				Token::TyName => {
					let name = l.token_sym(start);
					let ty = finish_parse_ty(l, start, effect, name)?;
					Ok(SelfEffectOrTy::Ty(ty))
				}
				other => Err(l.unexpected_token(start, other, b"'self', or type name")),
			}
		}
		Token::TyName => {
			let name = l.token_sym(start);
			let ty = finish_parse_ty(l, start, Effect::Pure, name)?;
			Ok(SelfEffectOrTy::Ty(ty))
		}
		other => Err(l.unexpected_token(start, other, b"'get', 'set', 'io', or type name")),
	}
}

pub fn parse_ty<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<ast::Ty<'a>> {
	let start = l.pos();
	let token = l.next_token();
	match token {
		Token::Get | Token::Set | Token::Io => {
			l.take_space()?;
			let effect = match token {
				Token::Get => Effect::Get,
				Token::Set => Effect::Set,
				Token::Io => Effect::Io,
				_ => panic!(),
			};
			let name = l.take_ty_name()?;
			finish_parse_ty(l, start, effect, name)
		}
		Token::TyName => {
			let name = l.token_sym(start);
			finish_parse_ty(l, start, Effect::Pure, name)
		}
		other => Err(l.unexpected_token(start, other, b"'get', 'set', 'io', or type name")),
	}
}

pub fn try_take_type_parameters<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<&'a [Sym]> {
	if !l.try_take_bracketl() {
		Ok(&[])
	} else {
		let b = l.direct_arr_builder::<Sym>();
		loop {
			&b <- l.take_ty_name()?;
			if l.try_take_bracketr() {
				break Ok(b.finish())
			}
			l.take_comma()?;
			l.take_space()?;
		}
	}
}

fn finish_parse_ty<'a, 't>(
	l: &mut Lexer<'a, 't>,
	start: Pos,
	effect: Effect,
	name: Sym,
) -> Result<ast::Ty<'a>> {
	let ty_args = try_take_type_arguments(l)?;
	Ok(ast::Ty { loc: l.loc_from(start), effect, name, ty_args })
}

pub fn try_take_type_argument<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<Option<ast::Ty<'a>>> {
	if l.try_take_bracketl() {
		let ty = parse_ty(l)?;
		l.take_bracketr()?;
		Ok(Some(ty))
	} else {
		Ok(None)
	}
}

pub fn try_take_type_arguments<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<List<'a, ast::Ty<'a>>> {
	if l.try_take_bracketl() {
		take_type_arguments_after_passing_bracketl(l)
	} else {
		Ok(List::empty())
	}
}

pub fn take_type_arguments_after_passing_bracketl<'a, 't>(
	l: &mut Lexer<'a, 't>,
) -> Result<List<'a, ast::Ty<'a>>> {
	let mut b = l.list_builder::<ast::Ty>();
	loop {
		&mut b <- parse_ty(l)?;
		if l.try_take_bracketr() {
			break Ok(b.finish())
		}
		l.take_comma()?;
		l.take_space()?;
	}
}
