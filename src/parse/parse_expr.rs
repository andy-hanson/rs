use diag::{ Diagnostic, DiagnosticData };
use model::LiteralValue;
use util::arr::{ Arr, ArrBuilder };
use util::loc::{ Loc, Pos };

use parse::ast;
use parse::lexer::{ Lexer, Result, CatchOrFinally };
use parse::parse_ty::{
	parse_ty,
	try_take_type_argument,
	try_take_type_arguments,
	take_type_arguments_after_passing_bracketl
};
use parse::token::Token;

pub fn parse_block(l: &mut Lexer) -> Result<ast::Expr> {
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
	NoOperator,
}

fn parse_block_with_start(l: &mut Lexer, start: Pos, first: Token) -> Result<ast::Expr> {
	let (expr, next) = parse_expr_2(l, Ctx::Statement, start, first)?;
	match next {
		Token::Newline =>
			match expr.data {
				ast::ExprData::LetInProgress(pattern, value) => {
					let then = parse_block(l)?;
					Ok(ast::Expr::let_expr(l.loc_from(start), pattern, value, then))
				}
				_ => {
					let then = parse_block(l)?;
					Ok(ast::Expr::seq(l.loc_from(start), expr, then))
				}
			},
		Token::Dedent =>
			Ok(expr),
		_ =>
			panic!(), // TODO: unexpected
	}
}

fn parse_expr_and_expect_next(l: &mut Lexer, ctx: Ctx, expected_next: Token) -> Result<ast::Expr> {
	let start = l.pos();
	let first_token = l.next_token();
	parse_expr_and_expect_next_2(l, ctx, expected_next, start, first_token)
}

fn parse_expr_and_expect_next_2(
	l: &mut Lexer, ctx: Ctx, expected_next: Token, start: Pos, start_token: Token) -> Result<ast::Expr> {
	let (expr, next) = parse_expr_2(l, ctx, start, start_token)?;
	if next != expected_next {
		panic!() // Diagnostic
	}
	Ok(expr)
}

fn parse_expr(l: &mut Lexer, ctx: Ctx) -> Result<(ast::Expr, Token)> {
	let start = l.pos();
	let first_token = l.next_token();
	parse_expr_2(l, ctx, start, first_token)
}

fn parse_expr_2(l: &mut Lexer, ctx: Ctx, start: Pos, first_token: Token) -> Result<(ast::Expr, Token)> {
	let (first, next) = parse_first_expr(l, start, first_token)?;
	match next {
		Token::Colon => {
			if ctx == Ctx::NoOperator {
				panic!()
			}

			l.take_space()?;
			let (args, next_2) = parse_args(l, Ctx::YesOperators)?;
			let call = ast::Expr::call(l.loc_from(start), first, args);
			Ok((call, next_2))
		}

		Token::Operator =>
			// In `f x + 1`, we would have read through the space while parsing the arguments to `f`.
			// So can encounter an operator now.
			if ctx == Ctx::NoOperator {
				Ok((first, Token::Operator))
			} else {
				slurp_operators(l, start, first)
			},

		Token::Space => {
			let next_start = l.pos();
			let tok = l.next_token();
			match tok {
				Token::Colon => {
					if ctx != Ctx::Statement {
						panic!()
					}
					l.take_equals()?;
					if let ast::ExprData::Access(property_name) = first.data {
						l.take_space()?;
						let (value, next_2) = parse_expr(l, Ctx::YesOperators)?;
						Ok((ast::Expr::set_property(l.loc_from(start), property_name, value), next_2))
					} else {
						Err(Diagnostic(first.loc, DiagnosticData::PrecedingEquals))
					}
				}

				Token::Equals => {
					if ctx != Ctx::Statement {
						panic!()
					}
					if let ast::ExprData::Access(local_name) = first.data {
						let pattern = ast::Pattern::single(first.loc, local_name);
						l.take_space()?;
						let (value, next_2) = parse_expr(l, Ctx::YesOperators)?;
						let loc = l.loc_from(start);
						if next_2 == Token::Newline {
							Ok((ast::Expr::let_in_progress(loc, pattern, value, ), Token::Newline))
						}
						else {
							Err(Diagnostic(loc, DiagnosticData::BlockCantEndInLet))
						}
					} else {
						Err(Diagnostic(first.loc, DiagnosticData::PrecedingEquals))
					}
				}

				Token::Then | Token::Else =>
					Ok((first, tok)),

				Token::Operator =>
					// If we are already on the RHS of an operator, don't continue parsing operators --
					// leave that to the outer version of `parseExprWithNext`.
					// This ensures that `a + b * c` is parsed as `(a + b) * c`,
					// because we stop parsing at the `*` and allow the outer parser to continue.
					if ctx == Ctx::NoOperator {
						Ok((first, Token::Operator))
					} else {
						slurp_operators(l, start, first)
					},

				_ => {
					let (args, next_2) = parse_args_2(l, Ctx::NoOperator, next_start, tok)?;
					let call = ast::Expr::call(l.loc_from(start), first, args);
					if ctx != Ctx::NoOperator && next_2 == Token::Operator {
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

fn parse_first_expr(l: &mut Lexer, start: Pos, token: Token) -> Result<(ast::Expr, Token)> {
	match token {
		Token::New => {
			// e.g. `new[Nat] 1, 2`
			let type_arguments = try_take_type_arguments(l)?;
			let ctx = if l.try_take_colon() { Ctx::YesOperators } else { Ctx::NoOperator };
			l.take_space()?;
			let (args, next) = parse_args(l, ctx)?;
			let expr = ast::Expr::new(l.loc_from(start), type_arguments, args);
			Ok((expr, next))
		}

		Token::Array => {
			let type_argument = try_take_type_argument(l)?;
			let ctx = if l.try_take_colon() { Ctx::YesOperators } else { Ctx::NoOperator };
			l.take_space()?;
			let (args, next) = parse_args(l, ctx)?;
			let expr = ast::Expr::array_literal(l.loc_from(start), type_argument, args);
			Ok((expr, next))
		}

		Token::Recur => {
			let ctx = if l.try_take_colon() { Ctx::YesOperators } else { Ctx::NoOperator };
			l.take_space()?;
			let (args, next) = parse_args(l, ctx)?;
			let expr = ast::Expr::recur(l.loc_from(start), args);
			Ok((expr, next))
		}

		Token::Assert => {
			l.take_space()?;
			let (asserted, next) = parse_expr(l, Ctx::YesOperators)?;
			let assert = ast::Expr::assert(l.loc_from(start), asserted);
			Ok((assert, next))
		}

		Token::If => {
			l.take_space()?;
			let test = parse_expr_and_expect_next(l, Ctx::YesOperators, Token::Then)?;
			l.take_space()?;
			let then = parse_expr_and_expect_next(l, Ctx::YesOperators, Token::Else)?;
			l.take_space()?;
			let (elze, next) = parse_expr(l, Ctx::YesOperators)?;
			let if_else = ast::Expr::if_else(l.loc_from(start), test, then, elze);
			Ok((if_else, next))
		}

		Token::For | Token::When | Token::Try => {
			let expr = match token {
				Token::For => parse_for(l, start),
				Token::When => parse_when(l, start),
				Token::Try => parse_try(l, start),
				_ => panic!()
			}?;
			let next = if l.try_take_dedent_from_dedenting() { Token::Dedent } else { Token::Newline };
			Ok((expr, next))
		}

		_ =>
			parse_simple_expr(l, start, token)
	}
}

fn parse_for(l: &mut Lexer, start: Pos) -> Result<ast::Expr> {
	l.take_space()?;
	let local_name = l.take_name()?;
	l.take_space()?;
	l.take_specific_keyword("in")?;
	l.take_space()?;
	let looper = parse_expr_and_expect_next(l, Ctx::YesOperators, Token::Indent)?;
	let body = parse_block(l)?;
	Ok(ast::Expr::for_expr(l.loc_from(start), local_name, looper, body))
}

fn slurp_operators(l: &mut Lexer, start: Pos, first: ast::Expr) -> Result<(ast::Expr, Token)> {
	let mut operator = l.token_sym();
	let mut left = first;
	loop {
		l.take_space()?; // operator must be followed by space.
		let (right, next_2) = parse_expr(l, Ctx::NoOperator)?;
		left = ast::Expr::operator_call(l.loc_from(start), left, operator, right);
		if next_2 == Token::Operator {
			operator = l.token_sym()
		} else {
			break Ok((left, next_2))
		}
	}
}

fn parse_args(l: &mut Lexer, ctx: Ctx) -> Result<(Arr<ast::Expr>, Token)> {
	let start = l.pos();
	let first_token = l.next_token();
	parse_args_2(l, ctx, start, first_token)
}

fn parse_args_2(l: &mut Lexer, ctx: Ctx, start: Pos, first_token: Token)
	-> Result<(Arr<ast::Expr>, Token)> {
	let mut args = ArrBuilder::<ast::Expr>::new();
	let (first_arg, mut next) = parse_expr_2(l, ctx, start, first_token)?;
	args.add(first_arg);
	while next == Token::Comma {
		l.take_space()?;
		let (next_arg, next_next) = parse_expr(l, ctx)?;
		args.add(next_arg);
		next = next_next
	}
	Ok((args.finish(), next))
}

fn parse_simple_expr(l: &mut Lexer, pos: Pos, token: Token) -> Result<(ast::Expr, Token)> {
	let mut expr = parse_simple_expr_without_suffixes(l, pos, token)?;

	loop {
		let start = l.pos();
		let next = l.next_token();
		match next {
			Token::Dot => {
				let name = l.take_name()?;
				expr = ast::Expr::get_property(l.loc_from(start), expr, name)
			}

			Token::BracketL => {
				let type_arguments = take_type_arguments_after_passing_bracketl(l)?;
				expr = ast::Expr::type_arguments(l.loc_from(start), expr, type_arguments)
			}

			Token::ParenL => {
				l.take_parenr()?;
				expr = ast::Expr::call(l.loc_from(start), expr, Arr::empty())
			}

			_ =>
				break Ok((expr, next))
		}
	}
}

fn parse_simple_expr_without_suffixes(l: &mut Lexer, pos: Pos, token: Token) -> Result<ast::Expr> {
	match token {
		Token::TyName => {
			let class_name = l.token_sym();
			l.take_dot()?;
			let static_method_name = l.take_name()?;
			Ok(ast::Expr::static_access(l.loc_from(pos), class_name, static_method_name))
		}
		Token::ParenL =>
			parse_expr_and_expect_next(l, Ctx::YesOperators, Token::ParenR),
		_ => {
			let loc = l.loc_from(pos);
			single_token_expr(l, loc, token)
		}
	}
}

fn single_token_expr(l: &mut Lexer, loc: Loc, token: Token) -> Result<ast::Expr> {
	match token {
		Token::Name =>
			Ok(ast::Expr::access(loc, l.token_sym())),
		Token::NatLiteral =>
			Ok(ast::Expr::literal(loc, LiteralValue::Nat(l.token_nat()))),
		Token::IntLiteral =>
			Ok(ast::Expr::literal(loc, LiteralValue::Int(l.token_int()))),
		Token::FloatLiteral =>
			Ok(ast::Expr::literal(loc, LiteralValue::Real(l.token_float()))),
		Token::StringLiteral =>
			Ok(ast::Expr::literal(loc, LiteralValue::String(l.token_string()))),
		Token::Pass =>
			Ok(ast::Expr::literal(loc, LiteralValue::Pass)),
		Token::True =>
			Ok(ast::Expr::literal(loc, LiteralValue::Bool(true))),
		Token::False =>
			Ok(ast::Expr::literal(loc, LiteralValue::Bool(false))),
		Token::SelfKw =>
			Ok(ast::Expr::self_expr(loc)),
		_ =>
			panic!() // TODO:diagnostic
	}
}

fn parse_when(l: &mut Lexer, start_pos: Pos) -> Result<ast::Expr> {
	/*
	when
		firstTest
			firstResult
		else
			elseResult
	*/
	l.take_indent()?;

	let mut cases = ArrBuilder::<ast::Case>::new();
	let mut case_start = start_pos;
	let mut case_start_token = l.next_token();
	loop {
		let first_test = parse_expr_and_expect_next_2(l, Ctx::YesOperators, Token::Indent, case_start, case_start_token)?;
		let first_result = parse_block(l)?;
		cases.add(ast::Case::of(l.loc_from(case_start), first_test, first_result));

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

	Ok(ast::Expr::when_test(l.loc_from(start_pos), cases.finish(), else_result))
}

fn parse_try(l: &mut Lexer, start_pos: Pos) -> Result<ast::Expr> {
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
	l.take_specific_keyword("do")?;
	l.take_indent()?;
	let do_ = parse_block(l)?;
	let mut catch: Option<ast::Catch> = None;
	let mut finally: Option<ast::Expr> = None;

	let catch_start = l.pos();

	match l.take_catch_or_finally()? {
		CatchOrFinally::Catch => {
			l.take_space()?;
			let exception_type = parse_ty(l)?;
			l.take_space()?;
			let name_start = l.pos();
			let exception_name = l.take_name()?;
			let name_loc = l.loc_from(name_start);
			l.take_indent()?;
			let catch_block = parse_block(l)?;
			catch = Some(ast::Catch::of(l.loc_from(catch_start), exception_type, name_loc, exception_name, catch_block));

			if !l.try_take_dedent()? {
				l.take_specific_keyword("finally")?;
				finally = Some(parse_finally(l)?);
			}
		}

		CatchOrFinally::Finally =>
			finally = Some(parse_finally(l)?)
	}

	Ok(ast::Expr::try(l.loc_from(start_pos), do_, catch, finally))
}

fn parse_finally(l: &mut Lexer) -> Result<ast::Expr> {
	l.take_indent()?;
	let res = parse_block(l)?;
	l.take_dedent()?;
	Ok(res)
}
