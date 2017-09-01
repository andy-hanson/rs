use util::arr::{Arr, ArrBuilder};
use util::either::Either;
use util::loc::Pos;
use util::path::{Path, RelPath};
use util::sym::Sym;

use compiler::model::effect::Effect;

use super::ast;
use super::lexer::{Lexer, MethodKw, NewlineOrDedent, NewlineOrIndent, Result, SlotKw};
use super::parse_expr::parse_block;
use super::parse_ty::{parse_self_effect_or_ty, parse_ty, try_take_type_parameters};
use super::token::Token;

pub fn parse_module(l: &mut Lexer) -> Result<ast::Module> {
	let kw = l.next_token();

	let (imports, class_start, next_kw): (Arr<ast::Import>, Pos, Token) = if kw == Token::Import {
		let imports = parse_imports(l)?;
		let class_start = l.pos();
		let next_kw = l.next_token();
		(imports, class_start, next_kw)
	} else {
		(Arr::empty(), l.pos(), kw)
	};

	let class = parse_class(l, class_start, next_kw)?;
	Ok(ast::Module::of(imports, class))
}

fn parse_imports(l: &mut Lexer) -> Result<Arr<ast::Import>> {
	let mut b = ArrBuilder::<ast::Import>::new();
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

		let mut path_parts = ArrBuilder::<Arr<u8>>::new();
		path_parts.add(l.take_ty_name_string()?);
		//TODO: really want to allow delving into grandchildren?
		while l.try_take_dot() {
			path_parts.add(l.take_ty_name_string()?)
		}

		let path = Path(path_parts.finish());
		let loc = l.loc_from(start_pos);
		b.add(
			if leading_dots == 0 {
				ast::Import::Global(loc, path)
			} else {
				ast::Import::Local(loc, RelPath::of(leading_dots, path))
			},
		)
	}
}

fn parse_class(l: &mut Lexer, start: Pos, kw: Token) -> Result<ast::Class> {
	let (type_parameters, start_2, kw_2) = try_parse_class_generic(l, start, kw)?;
	let (head, start_3, kw_3) = parse_head(l, start_2, kw_2)?;
	let (supers, start_4, kw_4) = parse_supers(l, start_3, kw_3)?;
	let methods = parse_methods(l, start_4, kw_4)?;
	Ok(ast::Class { loc: l.loc_from(start), type_parameters, head, supers, methods })
}

fn try_parse_class_generic(l: &mut Lexer, start: Pos, kw: Token) -> Result<(Arr<Sym>, Pos, Token)> {
	// e.g. `generic[T]`
	if kw != Token::Generic {
		return Ok((Arr::empty(), start, kw))
	}

	l.take_bracketl()?;
	let mut type_parameters = ArrBuilder::<Sym>::new();
	loop {
		type_parameters.add(l.take_ty_name()?);
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

fn parse_head(l: &mut Lexer, start: Pos, kw: Token) -> Result<(Option<ast::ClassHead>, Pos, MethodKw)> {
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
		_ => Err(l.unexpected_token(start, kw, "'abstract', 'static', 'slots', or 'enum'")),
	}
}

fn parse_abstract_head(l: &mut Lexer, start: Pos) -> Result<ast::ClassHead> {
	l.take_indent()?;
	let mut abstract_methods = ArrBuilder::<ast::AbstractMethod>::new();
	loop {
		let method_start = l.pos();
		let type_parameters = try_take_type_parameters(l)?;
		if type_parameters.any() {
			l.take_space()?;
		}
		let (return_ty, name, self_effect, parameters) = parse_method_head(l)?;
		abstract_methods.add(ast::AbstractMethod::of(
			l.loc_from(method_start),
			type_parameters,
			return_ty,
			name,
			self_effect,
			parameters,
		));
		match l.take_newline_or_dedent()? {
			NewlineOrDedent::Newline => {}
			NewlineOrDedent::Dedent => break,
		}
	}
	Ok(ast::ClassHead(l.loc_from(start), ast::ClassHeadData::Abstract(abstract_methods.finish())))
}

fn parse_methods(l: &mut Lexer, mut start: Pos, mut next: MethodKw) -> Result<Arr<ast::Method>> {
	let mut methods = ArrBuilder::<ast::Method>::new();
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
				methods.add(ast::Method {
					loc: l.loc_from(start),
					is_static,
					type_parameters,
					return_ty,
					name,
					self_effect,
					parameters,
					body,
				});
				start = l.pos();
				next = l.take_method_keyword_or_eof()?
			}
			MethodKw::Eof => break Ok(methods.finish()),
			MethodKw::Is => break Err(l.unexpected_token(start, Token::Is, "'def' or 'fun'")),
		}
	}
}

fn parse_method_head(l: &mut Lexer) -> Result<(ast::Ty, Sym, Effect, Arr<ast::Parameter>)> {
	let return_ty = parse_ty(l)?;
	l.take_space()?;
	let name = l.take_name()?;
	l.take_parenl()?;

	let (self_effect, parameters): (Effect, Arr<ast::Parameter>) = if l.try_take_parenr() {
		(Effect::Pure, Arr::empty())
	} else {
		let first_start = l.pos();
		match parse_self_effect_or_ty(l)? {
			Either::Left(self_effect) => {
				let parameters = if l.try_take_parenr() {
					Arr::empty()
				} else {
					parse_parameters(l, None)?
				};
				(self_effect, parameters)
			}
			Either::Right(first_ty) => {
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

fn parse_parameters(l: &mut Lexer, first: Option<ast::Parameter>) -> Result<Arr<ast::Parameter>> {
	let mut parameters = ArrBuilder::<ast::Parameter>::new_with_optional_first(first);
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
		parameters.add(ast::Parameter { loc: l.loc_from(start), ty, name })
	}
}

fn parse_supers(
	l: &mut Lexer,
	mut start: Pos,
	mut next: MethodKw,
) -> Result<(Arr<ast::Super>, Pos, MethodKw)> {
	let mut supers = ArrBuilder::<ast::Super>::new();
	loop {
		if next != MethodKw::Is {
			break Ok((supers.finish(), start, next))
		}

		supers.add(parse_super(l, start)?);

		start = l.pos();
		next = l.take_method_keyword_or_eof()?;
	}
}

fn parse_super(l: &mut Lexer, start: Pos) -> Result<ast::Super> {
	l.take_space()?;
	let name = l.take_ty_name()?;
	let ty_args = Arr::<ast::Ty>::empty(); // TODO
	let impls = match l.take_newline_or_indent()? {
		NewlineOrIndent::Indent => parse_impls(l)?,
		NewlineOrIndent::Newline => Arr::empty(),
	};
	Ok(ast::Super::of(l.loc_from(start), name, ty_args, impls))
}

fn parse_impls(l: &mut Lexer) -> Result<Arr<ast::Impl>> {
	let mut impls = ArrBuilder::<ast::Impl>::new();
	loop {
		// foo(x, y)
		let start = l.pos();
		let name = l.take_name()?;
		l.take_parenl()?;
		let parameter_names: Arr<Sym> = if l.try_take_parenr() {
			Arr::empty()
		} else {
			let first = l.take_name()?;
			let mut b = ArrBuilder::<Sym>::new_with_first(first);
			loop {
				if l.try_take_parenr() {
					break b.finish()
				}
				l.take_comma()?;
				l.take_space()?;
				b.add(l.take_name()?)
			}
		};

		let body = if l.try_take_indent()? { Some(parse_block(l)?) } else { None };

		impls.add(ast::Impl { loc: l.loc_from(start), name, parameter_names, body });
		if l.try_take_dedent_from_dedenting() {
			break Ok(impls.finish())
		}
	}
}

fn parse_slots(l: &mut Lexer, start: Pos) -> Result<ast::ClassHead> {
	l.take_indent()?;
	let mut slots = ArrBuilder::<ast::Slot>::new();
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
		slots.add(ast::Slot::of(l.loc_from(start), mutable, ty, name));
		match l.take_newline_or_dedent()? {
			NewlineOrDedent::Newline => {}
			NewlineOrDedent::Dedent => break,
		}
	}
	Ok(ast::ClassHead(l.loc_from(start), ast::ClassHeadData::Slots(slots.finish())))
}
