use util::arena::List;
use util::loc::Pos;

use super::super::super::model::diag::{Diag, Diagnostic};

use super::ast::{Case, Catch, Expr, ExprData, Pattern, PatternData, LiteralValue};
use super::lexer::{CatchOrFinally, Lexer, Next, Result};
use super::parse_ty::{parse_ty, take_type_arguments_after_passing_bracketl, try_take_type_argument,
                      try_take_type_arguments};
use super::token::Token;

pub fn parse_block<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<&'a Expr<'a>> {
	let start = l.pos();
	let next = l.next_token();
	parse_block_with_start(l, start, next)
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Ctx {
	Statement,
	// Allow any operator
	YesOperators,
	// Like YesOperators, but stop is you see an operator.
	NoOperators,
}
impl Ctx {
	fn operators_if(b: bool) -> Self {
		if b {
			Ctx::YesOperators
		} else {
			Ctx::NoOperators
		}
	}
}

fn parse_block_with_start<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, first: Token) -> Result<&'a Expr<'a>> {
	let (expr, next) = parse_expr_2(l, Ctx::Statement, start, first)?;
	match next.token {
		Token::Newline => {
			let data = match expr.1 {
				ExprData::LetInProgress(ref pattern, ref value) => {
					//TODO: would be nice to destroy the LetInProgress, but we already allocated it in the arena...
					let then = parse_block(l)?;
					ExprData::Let(pattern, value, then)
				}
				_ => {
					let then = parse_block(l)?;
					ExprData::Seq(expr, then)
				}
			};
			Ok(l.expr_from(start, data))
		}
		Token::Dedent => Ok(expr),
		_ => unimplemented!(), // TODO: unexpected
	}
}

fn parse_expr_and_expect_next<'a, 't>(l: &mut Lexer<'a, 't>, ctx: Ctx, expected_next: Token) -> Result<&'a Expr<'a>> {
	let start = l.pos();
	let first_token = l.next_token();
	parse_expr_and_expect_next_2(l, ctx, expected_next, start, first_token)
}

fn parse_expr_and_expect_next_2<'a, 't>(
	l: &mut Lexer<'a, 't>,
	ctx: Ctx,
	expected_next: Token,
	start: Pos,
	start_token: Token,
) -> Result<&'a Expr<'a>> {
	let (expr, next) = parse_expr_2(l, ctx, start, start_token)?;
	if next.token != expected_next {
		panic!() // Diagnostic
	}
	Ok(expr)
}

fn parse_expr<'a, 't>(l: &mut Lexer<'a, 't>, ctx: Ctx) -> Result<(&'a Expr<'a>, Next)> {
	let start = l.pos();
	let first_token = l.next_token();
	parse_expr_2(l, ctx, start, first_token)
}

fn parse_expr_2<'a, 't>(l: &mut Lexer<'a, 't>, ctx: Ctx, start: Pos, first_token: Token) -> Result<(&'a Expr<'a>, Next)> {
	let (first, next) = parse_first_expr(l, start, first_token)?;
	match next.token {
		Token::Colon => {
			if ctx == Ctx::NoOperators {
				panic!()
			}

			l.take_space()?;
			let (args, next_2) = parse_args(l, Ctx::YesOperators)?;
			let call = l.expr_from(start, ExprData::Call(first, args));
			Ok((call, next_2))
		}

		Token::Operator =>
			// In `f x + 1`, we would have read through the space while parsing the arguments to `f`.
			// So can encounter an operator now.
			if ctx == Ctx::NoOperators {
				Ok((first, next))
			} else {
				slurp_operators(l, start, first)
			},

		Token::Space => {
			let next = l.next_pos_token();
			match next.token {
				Token::Colon => {
					if ctx != Ctx::Statement {
						panic!()
					}
					l.take_equals()?;
					if let ExprData::Access(property_name) = first.1 {
						l.take_space()?;
						let (value, next_2) = parse_expr(l, Ctx::YesOperators)?;
						Ok((l.expr_from(start, ExprData::SetProperty(property_name, value)), next_2))
					} else {
						Err(Diagnostic(first.0, Diag::PrecedingEquals))
					}
				}

				Token::Equals => {
					if ctx != Ctx::Statement {
						panic!()
					}
					if let ExprData::Access(local_name) = first.1 {
						let pattern = Pattern(first.0, PatternData::Single(local_name));
						l.take_space()?;
						let (value, next_2) = parse_expr(l, Ctx::YesOperators)?;
						match next_2.token {
							Token::Newline =>
								Ok((l.expr_from(start, ExprData::LetInProgress(pattern, value)), next_2)),
							_ =>
								Err(Diagnostic(l.loc_from(start), Diag::BlockCantEndInLet)),
						}
					} else {
						Err(Diagnostic(first.0, Diag::PrecedingEquals))
					}
				}

				Token::Then | Token::Else =>
					Ok((first, next)),

				Token::Operator =>
					// If we are already on the RHS of an operator, don't continue parsing operators --
					// leave that to the outer version of `parseExprWithNext`.
					// This ensures that `a + b * c` is parsed as `(a + b) * c`,
					// because we stop parsing at the `*` and allow the outer parser to continue.
					if ctx == Ctx::NoOperators {
						Ok((first, next))
					} else {
						slurp_operators(l, start, first)
					},

				_ => {
					let (args, next_2) = parse_args_2(l, Ctx::NoOperators, next)?;
					let call = l.expr_from(start, ExprData::Call(first, args));
					if ctx != Ctx::NoOperators && next_2.token == Token::Operator {
						slurp_operators(l, start, call)
					} else {
						Ok((call, next_2))
					}
				}
			}
		}

		_ =>
			Ok((first, next))
	}
}

fn parse_first_expr<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, token: Token) -> Result<(&'a Expr<'a>, Next)> {
	let (data, next) = match token {
		Token::New => {
			// e.g. `new[Nat] 1, 2`
			let type_arguments = try_take_type_arguments(l)?;
			let ctx = Ctx::operators_if(l.try_take_colon());
			l.take_space()?;
			let (args, next) = parse_args(l, ctx)?;
			(ExprData::New(type_arguments, args), next)
		}

		Token::Array => {
			let type_argument = try_take_type_argument(l)?;
			let ctx = Ctx::operators_if(l.try_take_colon());
			l.take_space()?;
			let (args, next) = parse_args(l, ctx)?;
			(ExprData::ArrayLiteral(type_argument, args), next)
		}

		Token::Recur => {
			let ctx = Ctx::operators_if(l.try_take_colon());
			l.take_space()?;
			let (args, next) = parse_args(l, ctx)?;
			(ExprData::Recur(args), next)
		}

		Token::Assert => {
			l.take_space()?;
			let (asserted, next) = parse_expr(l, Ctx::YesOperators)?;
			(ExprData::Assert(asserted), next)
		}

		Token::If => {
			l.take_space()?;
			let test = parse_expr_and_expect_next(l, Ctx::YesOperators, Token::Then)?;
			l.take_space()?;
			let then = parse_expr_and_expect_next(l, Ctx::YesOperators, Token::Else)?;
			l.take_space()?;
			let (elze, next) = parse_expr(l, Ctx::YesOperators)?;
			(ExprData::IfElse(test, then, elze), next)
		}

		Token::For | Token::When | Token::Try => {
			let expr = match token {
				Token::For => parse_for(l, start),
				Token::When => parse_when(l, start),
				Token::Try => parse_try(l, start),
				_ => panic!(),
			}?;
			let next_pos = l.pos();
			let next_token = if l.try_take_dedent_from_dedenting() {
				Token::Dedent
			} else {
				Token::Newline
			};
			return Ok((expr, Next { pos: next_pos, token: next_token }))
		}

		_ => return parse_simple_expr(l, start, token),
	};
	Ok((l.expr_from(start, data), next))
}

fn parse_for<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos) -> Result<&'a Expr<'a>> {
	l.take_space()?;
	let local_name = l.take_name()?;
	l.take_space()?;
	l.take_specific_keyword(b"in")?;
	l.take_space()?;
	let looper = parse_expr_and_expect_next(l, Ctx::YesOperators, Token::Indent)?;
	let body = parse_block(l)?;
	Ok(l.expr_from(start, ExprData::For(local_name, looper, body)))
}

fn slurp_operators<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, first: &'a Expr) -> Result<(&'a Expr<'a>, Next)> {
	// Just saw Token::Operator
	let mut operator = l.token_sym(start);
	let mut left = first;
	loop {
		l.take_space()?; // operator must be followed by space.
		let (right, next) = parse_expr(l, Ctx::NoOperators)?;
		left = l.expr_from(start, ExprData::OperatorCall(left, operator, right));
		match next.token {
			Token::Operator => operator = l.token_sym(next.pos),
			_ => break Ok((left, next)),
		}
	}
}

fn parse_args<'a, 't>(l: &mut Lexer<'a, 't>, ctx: Ctx) -> Result<(List<'a, &'a Expr<'a>>, Next)> {
	let next = l.next_pos_token();
	parse_args_2(l, ctx, next)
}

fn parse_args_2<'a, 't>(l: &mut Lexer<'a, 't>,
	ctx: Ctx,
	Next { pos: start, token: first_token }: Next,
) -> Result<(List<'a, &'a Expr<'a>>, Next)> {
	let mut args = l.list_builder::<&'a Expr>(); //TODO:PERF use a List<Expr>, emplace new expressions into it
	let (first_arg, mut next) = parse_expr_2(l, ctx, start, first_token)?;
	args.add() <- first_arg;
	while next.token == Token::Comma {
		l.take_space()?;
		let (next_arg, next_next) = parse_expr(l, ctx)?;
		args.add() <- next_arg;
		next = next_next
	}
	Ok((args.finish(), next))
}

fn parse_simple_expr<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, token: Token) -> Result<(&'a Expr<'a>, Next)> {
	let mut expr = parse_simple_expr_without_suffixes(l, start, token)?;
	loop {
		let next = l.next_pos_token();
		let data = match next.token {
			Token::Dot => {
				let name = l.take_name()?;
				ExprData::GetProperty(expr, name)
			}
			Token::BracketL => {
				let type_arguments = take_type_arguments_after_passing_bracketl(l)?;
				ExprData::TypeArguments(expr, type_arguments)
			}
			Token::ParenL => {
				l.take_parenr()?;
				ExprData::Call(expr, List::empty())
			}
			_ => break Ok((expr, next)),
		};
		expr = l.expr_from(start, data)
	}
}

fn parse_simple_expr_without_suffixes<'a, 't>(l: &mut Lexer<'a, 't>, start: Pos, token: Token) -> Result<&'a Expr<'a>> {
	if token == Token::ParenL {
		return parse_expr_and_expect_next(l, Ctx::YesOperators, Token::ParenR)
	}

	let data = match token {
		Token::TyName => {
			let class_name = l.token_sym(start);
			l.take_dot()?;
			let static_method_name = l.take_name()?;
			ExprData::StaticAccess(class_name, static_method_name)
		}
		Token::Name => ExprData::Access(l.token_sym(start)),
		Token::NatLiteral => ExprData::Literal(LiteralValue::Nat(l.token_nat())),
		Token::IntLiteral => ExprData::Literal(LiteralValue::Int(l.token_int())),
		Token::FloatLiteral => ExprData::Literal(LiteralValue::Float(l.token_float())),
		Token::StringLiteral => ExprData::Literal(
			LiteralValue::String(l.quote_part_value())),
		Token::Pass => ExprData::Literal(LiteralValue::Pass),
		Token::True => ExprData::Literal(LiteralValue::Bool(true)),
		Token::False => ExprData::Literal(LiteralValue::Bool(false)),
		Token::SelfKw => ExprData::SelfExpr,
		_ => unimplemented!(), // TODO:diagnostic
	};
	Ok(l.expr_from(start, data))
}

fn parse_when<'a, 't>(l: &mut Lexer<'a, 't>, start_pos: Pos) -> Result<&'a Expr<'a>> {
	/*
	when
		firstTest
			firstResult
		else
			elseResult
	*/
	l.take_indent()?;

	let mut cases = l.list_builder::<Case>();
	let mut case_start = start_pos;
	let mut case_start_token = l.next_token();
	loop {
		let first_test =
			parse_expr_and_expect_next_2(l, Ctx::YesOperators, Token::Indent, case_start, case_start_token)?;
		let first_result = parse_block(l)?;
		cases.add() <- Case(l.loc_from(case_start), first_test, first_result);

		case_start = l.pos();
		case_start_token = l.next_token();
		if case_start_token != Token::Else {
			break
		}
	}

	l.take_indent()?;
	let else_result = parse_block(l)?;
	if !l.try_take_dedent_from_dedenting() {
		panic!() // 'else' must be the last clause. Must double-dedent after its block.
	}

	Ok(l.expr_from(start_pos, ExprData::WhenTest(cases.finish(), else_result)))
}

fn parse_try<'a, 't>(l: &mut Lexer<'a, 't>, start_pos: Pos) -> Result<&'a Expr<'a>> {
	/*
	try
		do
			...
		catch Exception e | optional
			...
		else | optional
			...
		finally | optional
			...
	*/
	l.take_indent()?;
	l.take_specific_keyword(b"do")?;
	l.take_indent()?;
	let do_ = parse_block(l)?;

	let catch_start = l.pos();

	let (catch, finally) = match l.take_catch_or_finally()? {
		CatchOrFinally::Catch => {
			l.take_space()?;
			let exception_type = parse_ty(l)?;
			l.take_space()?;
			let name_start = l.pos();
			let exception_name = l.take_name()?;
			let exception_name_loc = l.loc_from(name_start);
			l.take_indent()?;
			let then = parse_block(l)?;
			let catch = Catch {
				loc: l.loc_from(catch_start),
				exception_type,
				exception_name_loc,
				exception_name,
				then,
			};

			let finally = if !l.try_take_dedent()? {
				l.take_specific_keyword(b"finally")?;
				Some(parse_finally(l)?)
			} else { None };

			(Some(catch), finally)
		}

		CatchOrFinally::Finally => (None, Some(parse_finally(l)?)),
	};

	Ok(l.expr_from(start_pos, ExprData::Try(do_, catch, finally)))
}

fn parse_finally<'a, 't>(l: &mut Lexer<'a, 't>) -> Result<&'a Expr<'a>> {
	l.take_indent()?;
	let res = parse_block(l)?;
	l.take_dedent()?;
	Ok(res)
}
