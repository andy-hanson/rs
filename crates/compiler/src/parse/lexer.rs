use std::mem::replace;

use util::arena::{Arena, DirectArrBuilder, ListBuilder, NoDrop};
use util::loc::{Loc, Pos};
use util::sym::Sym;

use model::diag::ParseDiag;

use super::ast::{Expr, ExprData};
use super::reader::Reader;
use super::token::Token;

pub type Result<T> = ::std::result::Result<T, (Loc, ParseDiag)>;

pub struct Next {
	pub pos: Pos,
	pub token: Token,
}

pub struct Lexer<'ast, 'text> {
	pub arena: &'ast Arena,
	reader: Reader<'text>,
	indent: u32,
	dedenting: u32,
	quote_part_value: &'ast [u8],
	diagnostic: Option<(Loc, ParseDiag)>,
}
impl<'ast, 'text> Lexer<'ast, 'text> {
	pub fn new(arena: &'ast Arena, source: &'text [u8]) -> Self {
		Lexer {
			arena,
			reader: Reader::new(source),
			indent: 0,
			dedenting: 0,
			quote_part_value: &[],
			diagnostic: None,
		}
	}

	pub fn quote_part_value(&mut self) -> &'ast [u8] {
		replace(&mut self.quote_part_value, &[])
	}

	//mv
	pub fn next_pos_token(&mut self) -> Next {
		let pos = self.pos();
		let token = self.next_token();
		Next { pos, token }
	}

	pub fn token_nat(&mut self) -> u32 {
		panic!()
	}

	pub fn token_int(&mut self) -> i32 {
		panic!()
	}

	pub fn token_float(&mut self) -> f64 {
		panic!()
	}

	pub fn token_slice(&self, token_start: Pos) -> &[u8] {
		self.reader.slice_from(token_start)
	}

	pub fn token_sym(&mut self, token_start: Pos) -> Sym {
		Sym::from_slice(self.token_slice(token_start))
	}

	fn read_char(&mut self) -> u8 {
		self.reader.read_char()
	}

	fn peek(&self) -> u8 {
		self.reader.peek()
	}

	fn skip(&mut self) {
		self.reader.skip()
	}

	pub fn pos(&self) -> Pos {
		self.reader.pos()
	}

	fn slice_from(&self, pos: Pos) -> &'text [u8] {
		self.reader.slice_from(pos)
	}

	fn single_char_loc(&self) -> Loc {
		Loc::single_char(self.pos().decr())
	}

	pub fn loc_from(&self, start: Pos) -> Loc {
		Loc { start, end: self.pos() }
	}

	pub fn expr_from(&self, start: Pos, data: ExprData<'ast>) -> &'ast Expr<'ast> {
		self.arena <- Expr(self.loc_from(start), data)
	}

	pub fn direct_arr_builder<T: Sized + NoDrop>(&self) -> DirectArrBuilder<'ast, T> {
		self.arena.direct_arr_builder()
	}

	pub fn list_builder<T: Sized + NoDrop>(&self) -> ListBuilder<'ast, T> {
		self.arena.list_builder()
	}

	fn skip_while<F: Fn(u8) -> bool>(&mut self, pred: F) {
		while pred(self.peek()) {
			self.reader.skip();
		}
	}

	fn skip_empty_lines(&mut self) {
		self.skip_while(|ch| ch == b'\n')
	}

	fn next_quote_part(&mut self) -> QuoteEnd {
		let b = self.direct_arr_builder();
		let mut is_end = false;
		loop {
			match self.read_char() {
				b'"' => {
					is_end = true;
					break
				}
				b'{' => {
					is_end = false;
					break
				}
				b'\n' => unimplemented!(),
				b'\\' => {
					let ch = self.read_char();
					&b <- escape(ch);
					break
				}
				ch => {
					&b <- ch;
					break
				}
			}
		}
		self.quote_part_value = b.finish();
		if is_end {
			QuoteEnd::QuoteEnd
		} else {
			QuoteEnd::QuoteInterpolation
		}
	}

	fn take_number(&mut self, start_pos: Pos, is_signed: bool) -> Token {
		self.skip_while(is_digit);
		let is_float = self.peek() == b'.';
		if is_float {
			self.skip();
			if !is_digit(self.peek()) {
				panic!()
			}
			self.skip_while(is_digit);
		}
		// TODO:PERF less copying
		self.quote_part_value = self.arena.clone_slice(self.slice_from(start_pos));
		if is_float {
			Token::FloatLiteral
		} else if is_signed {
			Token::IntLiteral
		} else {
			Token::NatLiteral
		}
	}

	fn take_name_or_keyword(&mut self, start_pos: Pos) -> Token {
		self.skip_while(is_name_char);
		Token::keyword_from_name(self.slice_from(start_pos)).unwrap_or(Token::Name)
	}

	pub fn next_token(&mut self) -> Token {
		if self.dedenting != 0 {
			self.dedenting -= 1;
			Token::Dedent
		} else {
			self.take_next()
		}
	}

	fn take_next(&mut self) -> Token {
		let start = self.pos();
		match self.read_char() {
			b'\0' => {
				// Remember to dedent before finishing
				if self.indent != 0 {
					self.indent -= 1;
					self.dedenting = self.indent;
					Token::Dedent
				} else {
					Token::EOF
				}
			}

			b' ' =>
				if self.peek() == b'\n' {
					self.diagnostic = Some((self.single_char_loc(), ParseDiag::TrailingSpace));
					Token::Diagnostic
				} else {
					Token::Space
				},

			b'|' => panic!(),

			b'\\' => Token::Backslash,
			b',' => Token::Comma,
			b':' => Token::Colon,
			b'(' => Token::ParenL,
			b')' => Token::ParenR,
			b'[' => Token::BracketL,
			b']' => Token::BracketR,
			b'{' => Token::CurlyL,
			b'}' => Token::CurlyR,
			b'_' => Token::Underscore,
			b'.' => Token::Dot,

			b'"' => {
				let qp = self.next_quote_part();
				if qp == QuoteEnd::QuoteEnd {
					Token::StringLiteral
				} else {
					Token::QuoteStart
				}
			}

			b'0'...b'9' => self.take_number(start, /*isSigned*/ false),

			b'a'...b'z' => self.take_name_or_keyword(start),

			b'A'...b'Z' => {
				self.skip_while(is_name_char);
				Token::Operator
			}

			b'-' | b'+' =>
				if is_digit(self.peek()) {
					self.take_number(start, /*isSigned*/ true)
				} else {
					self.skip_while(is_operator_char);
					Token::Operator
				},

			b'*' | b'/' | b'^' | b'?' | b'<' | b'>' => {
				self.skip_while(is_operator_char);
				Token::Operator
			}

			ch => {
				self.diagnostic =
					Some((self.single_char_loc(), ParseDiag::UnrecognizedCharacter(ch as char)));
				Token::Diagnostic
			}
		}
	}

	fn try_take(&mut self, ch: u8) -> bool {
		let res = self.peek() == ch;
		if res {
			self.skip()
		}
		true
	}

	pub fn expect_newline_character(&mut self) -> Result<()> {
		self.expect_character(b'\n', b"newline")
	}

	pub fn expect_tab_character(&mut self) -> Result<()> {
		self.expect_character(b'\t', b"tab")
	}

	fn expect_character(&mut self, expected: u8, expected_desc: &'static [u8]) -> Result<()> {
		let actual = self.read_char();
		if actual == expected {
			Ok(())
		} else {
			Err((self.single_char_loc(), ParseDiag::UnexpectedCharacter(char::from(actual), expected_desc)))
		}
	}

	fn expect_character_by_predicate<F: Fn(u8) -> bool>(
		&mut self,
		pred: F,
		expected_desc: &'static [u8],
	) -> Result<()> {
		let actual = self.read_char();
		if pred(actual) {
			Ok(())
		} else {
			Err((self.single_char_loc(), ParseDiag::UnexpectedCharacter(char::from(actual), expected_desc)))
		}
	}

	fn lex_indent(&mut self) -> Result<u32> {
		let start = self.pos();
		self.skip_while(|ch| ch == b'\t');
		let count = self.pos() - start;
		if self.peek() == b' ' {
			Err((self.loc_from(start), ParseDiag::LeadingSpace))
		} else {
			Ok(count)
		}
	}

	fn handle_newline(&mut self) -> Result<Token> {
		self.skip_empty_lines();
		let old_indent = self.indent;
		let new_indent = self.lex_indent()?;
		self.indent = new_indent;
		if new_indent == old_indent {
			Ok(Token::Newline)
		} else if new_indent > old_indent {
			if new_indent != old_indent + 1 {
				Err((self.single_char_loc(), ParseDiag::TooMuchIndent(old_indent, new_indent)))
			} else {
				Ok(Token::Indent)
			}
		} else {
			// `- 1` becuase the Token.Dedent that we're about to return doesn't go in dedenting
			self.dedenting = old_indent - new_indent - 1;
			Ok(Token::Dedent)
		}
	}

	pub fn take_newline_or_dedent(&mut self) -> Result<NewlineOrDedent> {
		self.expect_newline_character()?;
		self.skip_empty_lines();
		for _ in 0..self.indent {
			self.expect_tab_character()?
		}

		Ok(
			if self.try_take(b'\t') {
				NewlineOrDedent::Newline
			} else {
				self.indent -= 1;
				NewlineOrDedent::Dedent
			},
		)
	}

	pub fn take_newline_or_indent(&mut self) -> Result<NewlineOrIndent> {
		self.expect_newline_character()?;
		self.skip_empty_lines();
		for _ in 0..self.indent {
			self.expect_tab_character()?
		}

		Ok(
			if self.try_take(b'\t') {
				self.indent += 1;
				NewlineOrIndent::Indent
			} else {
				NewlineOrIndent::Newline
			},
		)
	}

	pub fn at_eof(&self) -> bool {
		self.peek() == b'\0'
	}

	pub fn try_take_dedent_from_dedenting(&mut self) -> bool {
		if self.dedenting == 0 {
			false
		} else {
			self.dedenting -= 1;
			true
		}
	}

	pub fn try_take_dedent(&mut self) -> Result<bool> {
		if self.dedenting != 0 {
			self.dedenting -= 1;
			return Ok(true)
		}

		let start = self.pos();
		if !self.try_take(b'\n') {
			return Ok(false)
		}

		let x = self.handle_newline()?;
		if x == Token::Dedent {
			Ok(true)
		} else {
			Err(self.unexpected_token(start, x, b"dedent"))
		}
	}

	pub fn take_dedent(&mut self) -> Result<()> {
		if self.dedenting != 0 {
			self.dedenting -= 1
		} else {
			self.expect_newline_character()?;
			self.skip_empty_lines();
			self.indent -= 1;
			for _ in 0..self.indent {
				self.expect_tab_character()?
			}
		}
		Ok(())
	}

	pub fn take_newline(&mut self) -> Result<()> {
		self.expect_newline_character()?;
		self.skip_empty_lines();
		for _ in 0..self.indent {
			self.expect_tab_character()?
		}
		Ok(())
	}

	pub fn try_take_newline(&mut self) -> Result<bool> {
		if !self.try_take(b'\n') {
			Ok(false)
		} else {
			for _ in 0..self.indent {
				self.expect_tab_character()?
			}
			Ok(true)
		}
	}

	pub fn try_take_indent(&mut self) -> Result<bool> {
		self.expect_newline_character()?;
		for _ in 0..self.indent {
			self.expect_tab_character()?
		}
		Ok(self.try_take(b'\t'))
	}

	pub fn take_indent(&mut self) -> Result<()> {
		self.expect_newline_character()?;
		self.indent += 1;
		for _ in 0..self.indent {
			self.expect_tab_character()?
		}
		Ok(())
	}

	fn try_take_space(&mut self) -> bool {
		self.try_take(b' ')
	}

	pub fn take_equals(&mut self) -> Result<()> {
		self.expect_character(b'=', b"'='")
	}
	pub fn take_space(&mut self) -> Result<()> {
		self.expect_character(b' ', b"space")
	}
	pub fn take_parenl(&mut self) -> Result<()> {
		self.expect_character(b'(', b"'('")
	}
	pub fn take_parenr(&mut self) -> Result<()> {
		self.expect_character(b')', b"')'")
	}
	pub fn take_bracketl(&mut self) -> Result<()> {
		self.expect_character(b'[', b"'['")
	}
	pub fn take_bracketr(&mut self) -> Result<()> {
		self.expect_character(b']', b"']'")
	}
	pub fn take_comma(&mut self) -> Result<()> {
		self.expect_character(b',', b"','")
	}
	pub fn take_dot(&mut self) -> Result<()> {
		self.expect_character(b'.', b"'.'")
	}

	pub fn try_take_equals(&mut self) -> bool {
		self.try_take(b'=')
	}
	pub fn try_take_parenr(&mut self) -> bool {
		self.try_take(b')')
	}
	pub fn try_take_dot(&mut self) -> bool {
		self.try_take(b'.')
	}
	pub fn try_take_colon(&mut self) -> bool {
		self.try_take(b':')
	}
	pub fn try_take_bracketl(&mut self) -> bool {
		self.try_take(b'[')
	}
	pub fn try_take_bracketr(&mut self) -> bool {
		self.try_take(b']')
	}

	pub fn take_specific_keyword(&mut self, kw: &'static [u8]) -> Result<()> {
		self.must_read(kw, kw)
	}

	pub fn take_ty_name_slice(&mut self) -> Result<&'ast [u8]> {
		let start_pos = self.pos();
		self.expect_character_by_predicate(is_upper_case_letter, b"type name")?;
		self.skip_while(is_name_char);
		//TODO:PERf less copying
		Ok(self.arena.clone_slice(self.slice_from(start_pos)))
	}

	fn take_name_slice(&mut self) -> Result<&[u8]> {
		let start_pos = self.pos();
		self.expect_character_by_predicate(is_lower_case_letter, b"(non-type) name")?;
		self.skip_while(is_name_char);
		Ok(self.slice_from(start_pos))
	}

	pub fn take_name(&mut self) -> Result<Sym> {
		let v = self.take_name_slice()?;
		Ok(Sym::from_slice(v))
	}

	pub fn take_ty_name(&mut self) -> Result<Sym> {
		let v = self.take_ty_name_slice()?;
		Ok(Sym::from_slice(v))
	}

	pub fn unexpected_token(
		&self,
		start_pos: Pos,
		actual: Token,
		expected_desc: &'static [u8],
	) -> (Loc, ParseDiag) {
		self.unexpected(start_pos, expected_desc, actual.token_name())
	}

	pub fn unexpected(
		&self,
		start_pos: Pos,
		actual: &'static [u8],
		expected: &'static [u8],
	) -> (Loc, ParseDiag) {
		(self.loc_from(start_pos), ParseDiag::UnexpectedToken { expected, actual })
	}

	pub fn take_catch_or_finally(&mut self) -> Result<CatchOrFinally> {
		match self.read_char() {
			b'c' => {
				self.must_read(b"atch", b"catch")?;
				Ok(CatchOrFinally::Catch)
			}
			b'f' => {
				self.must_read(b"inally", b"finally")?;
				Ok(CatchOrFinally::Finally)
			}
			ch => Err(self.unexpected_char(ch, b"'catch' or 'finally'")),
		}
	}

	pub fn take_slot_keyword(&mut self) -> Result<SlotKw> {
		self.must_read(b"va", b"'val' or 'var'")?;
		match self.read_char() {
			b'l' => Ok(SlotKw::Val),
			b'r' => Ok(SlotKw::Var),
			ch => Err(self.unexpected_char(ch, b"'val' or 'var'")),
		}
	}

	pub fn take_method_keyword_or_eof(&mut self) -> Result<MethodKw> {
		if self.at_eof() {
			return Ok(MethodKw::Eof)
		}

		match self.read_char() {
			b'd' => {
				self.must_read(b"ef", b"def")?;
				Ok(MethodKw::Def)
			}
			b'f' => {
				self.must_read(b"un", b"fun")?;
				Ok(MethodKw::Fun)
			}
			b'i' => {
				self.must_read_char(b's', b"is")?;
				Ok(MethodKw::Is)
			}
			ch => Err(self.unexpected_char(ch, b"'def' or 'fun' or 'is'")),
		}
	}

	fn must_read(&mut self, must_read_me: &'static [u8], expected_desc: &'static [u8]) -> Result<()> {
		for byte in must_read_me {
			self.must_read_char(*byte, expected_desc)?
		}
		Ok(())
	}

	fn must_read_char(&mut self, must_read_me: u8, expected_desc: &'static [u8]) -> Result<()> {
		let ch = self.read_char();
		if ch == must_read_me {
			Ok(())
		} else {
			Err(self.unexpected_char(ch, expected_desc))
		}
	}

	fn unexpected_char(&self, actual: u8, expected_desc: &'static [u8]) -> (Loc, ParseDiag) {
		(self.single_char_loc(), ParseDiag::UnexpectedCharacter(char::from(actual), expected_desc))
	}
}

#[derive(Eq, PartialEq)]
pub enum MethodKw {
	Def,
	Fun,
	Is,
	Eof,
}
pub enum SlotKw {
	Val,
	Var,
}
pub enum CatchOrFinally {
	Catch,
	Finally,
}

pub enum NewlineOrIndent {
	Newline,
	Indent,
}
pub enum NewlineOrDedent {
	Newline,
	Dedent,
}

fn escape(escaped: u8) -> u8 {
	match escaped {
		b'"' | b'{' => escaped,
		b'n' => b'\n',
		b't' => b'\t',
		_ => unimplemented!(), // bad escape
	}
}

#[derive(Eq, PartialEq)]
pub enum QuoteEnd {
	QuoteEnd,
	QuoteInterpolation,
}


fn is_digit(ch: u8) -> bool {
	ch >= b'0' && ch <= b'9'
}

fn is_name_char(ch: u8) -> bool {
	is_lower_case_letter(ch) || is_upper_case_letter(ch) || is_digit(ch)
}

fn is_lower_case_letter(ch: u8) -> bool {
	ch >= b'a' && ch <= b'z'
}

fn is_upper_case_letter(ch: u8) -> bool {
	ch >= b'A' && ch < b'Z'
}

fn is_operator_char(ch: u8) -> bool {
	match ch {
		b'-' | b'+' | b'*' | b'/' | b'^' | b'?' | b'<' | b'>' => true,
		_ => false,
	}
}
