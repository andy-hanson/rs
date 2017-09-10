use util::arena::List;
use util::arr::SliceOps;
use util::loc::Pos;
use util::path::{Path, RelPath};
use util::sym::Sym;

use model::effect::Effect;

use super::ast::{AbstractMethod, Class, ClassHead, ClassHeadData, Expr, Impl, Import, Method, Module,
                 Parameter, Slot, Super, Ty};
use super::lexer::{Lexer, MethodKw, NewlineOrDedent, NewlineOrIndent, Result, SlotKw};
use super::parse_expr::parse_block;
use super::parse_ty::{parse_self_effect_or_ty, parse_ty, try_take_type_parameters, SelfEffectOrTy};
use super::token::Token;

pub fn parse_module<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<Module<'a>> {
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
	Ok(Module { imports, class })
}

fn parse_imports<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<List<'a, Import<'a>>> {
	let mut b = l.list_builder::<Import>();
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


		let path = parse_path(l); //path_parts.finish();
		let loc = l.loc_from(start_pos);
		&mut b <-
			if leading_dots == 0 {
				Import::Global(loc, path)
			} else {
				Import::Local(loc, RelPath { n_parents: leading_dots - 1, rel_to_parent: path })
			};
	}
}

fn parse_path<'a, 't>(l: &mut Lexer<'a, 't>) -> Path<'a> {
	unused!(l);
	/*let mut path_parts: ListBuilder<'a, &'a [u8]> = l.list_builder::<&'a [u8]>();
	path_parts.add() <- l.take_ty_name_slice()?;
	//TODO: really want to allow delving into grandchildren?
	while l.try_take_dot() {
		path_parts.add() <- l.take_ty_name_slice()?;
	}*/
	unimplemented!()
}

fn parse_class<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, kw: Token) -> Result<&'a Class<'a>> {
	let (type_parameters, start_2, kw_2) = try_parse_class_generic(l, start, kw)?;
	let (head, start_3, kw_3) = parse_head(l, start_2, kw_2)?;
	let (supers, start_4, kw_4) = parse_supers(l, start_3, kw_3)?;
	let methods = parse_methods(l, start_4, kw_4)?;
	Ok(l.arena <- Class { loc: l.loc_from(start), type_parameters, head, supers, methods })
}

fn try_parse_class_generic<'a, 't>(
	l: &mut Lexer<'a, 't>,
	start: Pos,
	kw: Token,
) -> Result<(&'a [Sym], Pos, Token)> {
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

fn parse_head<'a, 't>(
	l: &mut Lexer<'a, 't>,
	start: Pos,
	kw: Token,
) -> Result<(Option<ClassHead<'a>>, Pos, MethodKw)> {
	match kw {
		Token::EOF => Ok((None, start, MethodKw::Eof)),
		Token::Fun => Ok((None, start, MethodKw::Fun)),
		Token::Builtin => {
			let head = ClassHead(l.loc_from(start), ClassHeadData::Builtin);
			l.take_newline()?;
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
		_ => Err(l.unexpected_token(start, kw, b"'builtin', 'abstract', 'static', 'slots', or 'enum'")),
	}
}

fn parse_abstract_head<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<ClassHead<'a>> {
	l.take_indent()?;
	let mut abstract_methods = l.list_builder::<AbstractMethod>();
	loop {
		let method_start = l.pos();
		let type_parameters = try_take_type_parameters(l)?;
		if type_parameters.any() {
			l.take_space()?;
		}
		let (return_ty, name, self_effect, parameters) = parse_method_head(l)?;
		&mut abstract_methods <- AbstractMethod {
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
	Ok(ClassHead(l.loc_from(start), ClassHeadData::Abstract(abstract_methods.finish())))
}

fn parse_methods<'a, 't>(
	l: &mut Lexer<'a, 't>,
	mut start: Pos,
	mut next: MethodKw,
) -> Result<List<'a, Method<'a>>> {
	let mut methods = l.list_builder::<Method>();
	loop {
		match next {
			MethodKw::Def | MethodKw::Fun => {
				let is_static = next == MethodKw::Fun;
				let type_parameters = try_take_type_parameters(l)?;
				l.take_space()?;
				let (return_ty, name, self_effect, parameters) = parse_method_head(l)?;
				let body = take_optional_body(l)?;
				&mut methods <- Method {
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

fn parse_method_head<'a, 't>(
	l: &mut Lexer<'a, 't>,
) -> Result<(Ty<'a>, Sym, Effect, List<'a, Parameter<'a>>)> {
	let return_ty = parse_ty(l)?;
	l.take_space()?;
	let name = l.take_name_or_operator()?;
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
				let first = Parameter { loc: l.loc_from(first_start), ty: first_ty, name: first_name };
				let parameters = parse_parameters(l, Some(first))?;
				(Effect::Pure, parameters)
			}
		}
	};

	Ok((return_ty, name, self_effect, parameters))
}

fn parse_parameters<'a, 't>(
	l: &mut Lexer<'a, 't>,
	first: Option<Parameter<'a>>,
) -> Result<List<'a, Parameter<'a>>> {
	let mut parameters = l.list_builder::<Parameter<'a>>();
	if let Some(f) = first {
		&mut parameters <- f;
	}
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
		&mut parameters <- Parameter { loc: l.loc_from(start), ty, name };
	}
}

fn parse_supers<'a, 't>(
	l: &mut Lexer<'a, 't>,
	mut start: Pos,
	mut next: MethodKw,
) -> Result<(List<'a, Super<'a>>, Pos, MethodKw)> {
	let mut supers = l.list_builder::<Super>();
	loop {
		if next != MethodKw::Is {
			break Ok((supers.finish(), start, next))
		}

		&mut supers <- parse_super(l, start)?;

		start = l.pos();
		next = l.take_method_keyword_or_eof()?;
	}
}

fn parse_super<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<Super<'a>> {
	l.take_space()?;
	let name = l.take_ty_name()?;
	let ty_args = List::empty(); // TODO
	let impls = match l.take_newline_or_indent()? {
		NewlineOrIndent::Indent => parse_impls(l)?,
		NewlineOrIndent::Newline => List::empty(),
	};
	Ok(Super { loc: l.loc_from(start), name, ty_args, impls })
}

//mv
fn take_optional_body<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<Option<&'a Expr<'a>>> {
	Ok(match l.take_newline_or_indent()? {
		NewlineOrIndent::Indent => Some(parse_block(l)?),
		NewlineOrIndent::Newline => None,
	})
}

fn parse_impls<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<List<'a, Impl<'a>>> {
	let mut impls = l.list_builder::<Impl<'a>>();
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

		let body = take_optional_body(l)?;
		&mut impls <- Impl { loc: l.loc_from(start), name, parameter_names, body };
		if l.try_take_dedent_from_dedenting() {
			break Ok(impls.finish())
		}
	}
}

fn parse_slots<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<ClassHead<'a>> {
	l.take_indent()?;
	let mut slots = l.list_builder::<Slot>();
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
		&mut slots <- Slot { loc: l.loc_from(start), mutable, ty, name };
		match l.take_newline_or_dedent()? {
			NewlineOrDedent::Newline => {}
			NewlineOrDedent::Dedent => break,
		}
	}
	Ok(ClassHead(l.loc_from(start), ClassHeadData::Slots(slots.finish())))
}
