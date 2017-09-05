use util::arena::{List, ListBuilder};
use util::arr::{SliceOps};
use util::loc::Pos;
use util::sym::Sym;

use super::super::super::model::effect::Effect;

use super::ast;
use super::lexer::{Lexer, MethodKw, NewlineOrDedent, NewlineOrIndent, Result, SlotKw};
use super::parse_expr::parse_block;
use super::parse_ty::{parse_self_effect_or_ty, parse_ty, try_take_type_parameters, SelfEffectOrTy};
use super::token::Token;

pub fn parse_module<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<ast::Module<'a>> {
	let kw = l.next_token();

	let (imports, class_start, next_kw) = if kw == Token::Import {
		let imports = parse_imports(l)?;
		let class_start = l.pos();
		let next_kw = l.next_token();
		(imports, class_start, next_kw)
	} else {
		(List::empty(), l.pos(), kw)
	};

	let class = parse_class(l, class_start, next_kw)?;
	Ok(ast::Module { imports, class })
}

fn parse_imports<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<List<'a, ast::Import<'a>>> {
	let mut b = l.list_builder::<ast::Import>();
	loop {
		if l.try_take_newline()? {
			break Ok(b.finish())
		}

		l.take_space()?;

		let start_pos = l.pos();
		let mut leading_dots = 0;
		while l.try_take_dot() {
			leading_dots += 1
		}

		let mut path_parts: ListBuilder<'a, &'a [u8]> = l.list_builder::<&'a [u8]>();
		path_parts.add() <- l.take_ty_name_slice()?;
		//TODO: really want to allow delving into grandchildren?
		while l.try_take_dot() {
			path_parts.add() <- l.take_ty_name_slice()?;
		}

		let path = path_parts.finish();
		let loc = l.loc_from(start_pos);
		b.add() <-
			if leading_dots == 0 {
				ast::Import::Global(loc, path)
			} else {
				ast::Import::Local(loc,leading_dots - 1, path)
			};
	}
}

fn parse_class<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, kw: Token) -> Result<&'a ast::Class<'a>> {
	let (type_parameters, start_2, kw_2) = try_parse_class_generic(l, start, kw)?;
	let (head, start_3, kw_3) = parse_head(l, start_2, kw_2)?;
	let (supers, start_4, kw_4) = parse_supers(l, start_3, kw_3)?;
	let methods = parse_methods(l, start_4, kw_4)?;
	Ok(l.arena <- ast::Class { loc: l.loc_from(start), type_parameters, head, supers, methods })
}

fn try_parse_class_generic<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, kw: Token) -> Result<(&'a [Sym], Pos, Token)> {
	// e.g. `generic[T]`
	if kw != Token::Generic {
		return Ok((&[], start, kw))
	}

	l.take_bracketl()?;
	let type_parameters = l.direct_arr_builder::<Sym>();
	loop {
		&type_parameters <- l.take_ty_name()?;
		let is_next = !l.try_take_bracketr();
		if !is_next {
			break
		}
		l.take_comma()?;
		l.take_space()?;
	}
	l.take_newline()?;
	let next_pos = l.pos();
	let next_tok = l.next_token();
	Ok((type_parameters.finish(), next_pos, next_tok))
}

fn parse_head<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, kw: Token) -> Result<(Option<ast::ClassHead<'a>>, Pos, MethodKw)> {
	match kw {
		Token::EOF => Ok((None, start, MethodKw::Eof)),
		Token::Fun => Ok((None, start, MethodKw::Fun)),
		Token::Builtin => {
			let head = ast::ClassHead(l.loc_from(start), ast::ClassHeadData::Builtin);
			Ok((Some(head), l.pos(), l.take_method_keyword_or_eof()?))
		}
		Token::Abstract => {
			let head = parse_abstract_head(l, start)?;
			Ok((Some(head), l.pos(), l.take_method_keyword_or_eof()?))
		}
		Token::Slots => {
			let head = parse_slots(l, start)?;
			Ok((Some(head), l.pos(), l.take_method_keyword_or_eof()?))
		}
		Token::Enum => panic!(), // TODO
		_ => Err(l.unexpected_token(start, kw, b"'abstract', 'static', 'slots', or 'enum'")),
	}
}

fn parse_abstract_head<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<ast::ClassHead<'a>> {
	l.take_indent()?;
	let mut abstract_methods = l.list_builder::<ast::AbstractMethod>();
	loop {
		let method_start = l.pos();
		let type_parameters = try_take_type_parameters(l)?;
		if type_parameters.any() {
			l.take_space()?;
		}
		let (return_ty, name, self_effect, parameters) = parse_method_head(l)?;
		abstract_methods.add() <- ast::AbstractMethod {
			loc: l.loc_from(method_start),
			type_parameters,
			return_ty,
			name,
			self_effect,
			parameters,
		};
		match l.take_newline_or_dedent()? {
			NewlineOrDedent::Newline => {}
			NewlineOrDedent::Dedent => break,
		}
	}
	Ok(ast::ClassHead(l.loc_from(start), ast::ClassHeadData::Abstract(abstract_methods.finish())))
}

fn parse_methods<'a, 't>(l: &mut Lexer<'a, 't>, mut start: Pos, mut next: MethodKw) -> Result<List<'a, ast::Method<'a>>> {
	let mut methods = l.list_builder::<ast::Method>();
	loop {
		match next {
			MethodKw::Def | MethodKw::Fun => {
				let is_static = next == MethodKw::Fun;
				let type_parameters = try_take_type_parameters(l)?;
				l.take_space()?;
				let (return_ty, name, self_effect, parameters) = parse_method_head(l)?;
				let body = if l.try_take_indent()? {
					Some(parse_block(l)?)
				} else {
					None
				};
				methods.add() <- ast::Method {
					loc: l.loc_from(start),
					is_static,
					type_parameters,
					return_ty,
					name,
					self_effect,
					parameters,
					body,
				};
				start = l.pos();
				next = l.take_method_keyword_or_eof()?
			}
			MethodKw::Eof => break Ok(methods.finish()),
			MethodKw::Is => break Err(l.unexpected_token(start, Token::Is, b"'def' or 'fun'")),
		}
	}
}

fn parse_method_head<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<(ast::Ty<'a>, Sym, Effect, List<'a, ast::Parameter<'a>>)> {
	let return_ty = parse_ty(l)?;
	l.take_space()?;
	let name = l.take_name()?;
	l.take_parenl()?;

	let (self_effect, parameters) = if l.try_take_parenr() {
		(Effect::Pure, List::empty())
	} else {
		let first_start = l.pos();
		match parse_self_effect_or_ty(l)? {
			SelfEffectOrTy::SelfEffect(self_effect) => {
				let parameters = if l.try_take_parenr() {
					List::empty()
				} else {
					parse_parameters(l, None)?
				};
				(self_effect, parameters)
			}
			SelfEffectOrTy::Ty(first_ty) => {
				l.take_space()?;
				let first_name = l.take_name()?;
				let first = ast::Parameter { loc: l.loc_from(first_start), ty: first_ty, name: first_name };
				let parameters = parse_parameters(l, Some(first))?;
				(Effect::Pure, parameters)
			}
		}
	};

	Ok((return_ty, name, self_effect, parameters))
}

fn parse_parameters<'a, 't>(l: &mut Lexer<'a, 't>, first: Option<ast::Parameter<'a>>) -> Result<List<'a, ast::Parameter<'a>>> {
	let mut parameters = l.list_builder::<ast::Parameter<'a>>();
	if let Some(f) = first { parameters.add() <- f; }
	loop {
		if l.try_take_parenr() {
			break Ok(parameters.finish())
		}
		l.take_comma()?;
		l.take_space()?;
		let start = l.pos();
		let ty = parse_ty(l)?;
		l.take_space()?;
		let name = l.take_name()?;
		parameters.add() <- ast::Parameter { loc: l.loc_from(start), ty, name };
	}
}

fn parse_supers<'a, 't>(
	l: &mut Lexer<'a, 't>,
	mut start: Pos,
	mut next: MethodKw,
) -> Result<(List<'a, ast::Super<'a>>, Pos, MethodKw)> {
	let mut supers = l.list_builder::<ast::Super>();
	loop {
		if next != MethodKw::Is {
			break Ok((supers.finish(), start, next))
		}

		supers.add() <- parse_super(l, start)?;

		start = l.pos();
		next = l.take_method_keyword_or_eof()?;
	}
}

fn parse_super<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<ast::Super<'a>> {
	l.take_space()?;
	let name = l.take_ty_name()?;
	let ty_args = List::empty(); // TODO
	let impls = match l.take_newline_or_indent()? {
		NewlineOrIndent::Indent => parse_impls(l)?,
		NewlineOrIndent::Newline => List::empty(),
	};
	Ok(ast::Super { loc: l.loc_from(start), name, ty_args, impls })
}

fn parse_impls<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<List<'a, ast::Impl<'a>>> {
	let mut impls = l.list_builder::<ast::Impl<'a>>();
	loop {
		// foo(x, y)
		let start = l.pos();
		let name = l.take_name()?;
		l.take_parenl()?;
		let parameter_names: &'a [Sym] = if l.try_take_parenr() {
			&[]
		} else {
			let b = l.direct_arr_builder::<Sym>();
			&b <- l.take_name()?;
			loop {
				if l.try_take_parenr() {
					break b.finish()
				}
				l.take_comma()?;
				l.take_space()?;
				&b <- l.take_name()?;
			}
		};

		let body = if l.try_take_indent()? {
			Some(parse_block(l)?)
		} else {
			None
		};

		impls.add() <- ast::Impl { loc: l.loc_from(start), name, parameter_names, body };
		if l.try_take_dedent_from_dedenting() {
			break Ok(impls.finish())
		}
	}
}

fn parse_slots<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<ast::ClassHead<'a>> {
	l.take_indent()?;
	let mut slots = l.list_builder::<ast::Slot>();
	loop {
		let start = l.pos();
		let next = l.take_slot_keyword()?;
		let mutable = match next {
			SlotKw::Val => false,
			SlotKw::Var => true,
		};
		l.take_space()?;
		let ty = parse_ty(l)?;
		l.take_space()?;
		let name = l.take_name()?;
		slots.add() <- ast::Slot { loc: l.loc_from(start), mutable, ty, name };
		match l.take_newline_or_dedent()? {
			NewlineOrDedent::Newline => {}
			NewlineOrDedent::Dedent => break,
		}
	}
	Ok(ast::ClassHead(l.loc_from(start), ast::ClassHeadData::Slots(slots.finish())))
}
