use compiler::model::effect::Effect;

use util::arr::{ Arr, ArrBuilder };
use util::either::Either;
use util::loc::{ Pos };
use util::sym::Sym;

use super::ast;
use super::lexer::{ Lexer, Result };
use super::token::Token;

pub fn parse_self_effect_or_ty(l: &mut Lexer) -> Result<Either<Effect, ast::Ty>> {
	let start = l.pos();
	let token = l.next_token();
	match token {
		Token::Get | Token::Set | Token::Io => {
			l.take_space()?;
			let effect = match token { Token::Get => Effect::Get, Token::Set => Effect::Set, Token::Io => Effect::Io, _ => panic!() };
			match l.next_token() {
				Token::SelfKw =>
					Ok(Either::Left(effect)),
				Token::TyName => {
					let name = l.token_sym(start);
					let ty = finish_parse_ty(l, start, effect, name)?;
					Ok(Either::Right(ty))
				}
				other =>
					Err(l.unexpected_token(start, other, "'self', or type name"))
			}
		}
		Token::TyName => {
			let name = l.token_sym(start);
			let ty = finish_parse_ty(l, start, Effect::Pure, name)?;
			Ok(Either::Right(ty))
		}
		other =>
			Err(l.unexpected_token(start, other, "'get', 'set', 'io', or type name"))
	}
}

pub fn parse_ty(l: &mut Lexer) -> Result<ast::Ty> {
	let start = l.pos();
	let token = l.next_token();
	match token {
		Token::Get | Token::Set | Token::Io => {
			l.take_space()?;
			let effect = match token { Token::Get => Effect::Get, Token::Set => Effect::Set, Token::Io => Effect::Io, _ => panic!() };
			let name = l.take_ty_name()?;
			finish_parse_ty(l, start, effect, name)
		}
		Token::TyName => {
			let name = l.token_sym(start);
			finish_parse_ty(l, start, Effect::Pure, name)
		}
		other =>
			Err(l.unexpected_token(start, other, "'get', 'set', 'io', or type name"))
	}
}

pub fn try_take_type_parameters(l: &mut Lexer) -> Result<Arr<Sym>> {
	if !l.try_take_bracketl() {
		Ok(Arr::empty())
	} else {
		let mut b = ArrBuilder::<Sym>::new();
		loop {
			b.add(l.take_ty_name()?);
			if l.try_take_bracketr() {
				break Ok(b.finish())
			}
			l.take_comma()?;
			l.take_space()?;
		}
	}
}

fn finish_parse_ty(l: &mut Lexer, start: Pos, effect: Effect, name: Sym) -> Result<ast::Ty> {
	let ty_args = try_take_type_arguments(l)?;
	Ok(ast::Ty::of(l.loc_from(start), effect, name, ty_args))
}

pub fn try_take_type_argument(l: &mut Lexer) -> Result<Option<ast::Ty>> {
	if l.try_take_bracketl() {
		let ty = parse_ty(l)?;
		l.take_bracketr()?;
		Ok(Some(ty))
	} else {
		Ok(None)
	}
}

pub fn try_take_type_arguments(l: &mut Lexer) -> Result<Arr<ast::Ty>> {
	if l.try_take_bracketl() {
		take_type_arguments_after_passing_bracketl(l)
	} else {
		Ok(Arr::empty())
	}
}

pub fn take_type_arguments_after_passing_bracketl(l: &mut Lexer) -> Result<Arr<ast::Ty>> {
	let mut b = ArrBuilder::<ast::Ty>::new();
	loop {
		b.add(parse_ty(l)?);
		if l.try_take_bracketr() {
			break Ok(b.finish())
		}
		l.take_comma()?;
		l.take_space()?;
	}
}

