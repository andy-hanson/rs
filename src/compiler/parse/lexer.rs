use std::cell::Cell;
use std::mem::replace;
use std::slice::Iter;

use compiler::diag::{ Diagnostic, DiagnosticData };
use util::arr::{ Arr, ArrBuilder };
use util::ascii;
use util::ascii::Ascii;
use util::loc::{ Pos, Loc, POS_ZERO };
use util::sym::Sym;

use super::token::Token;

pub type Result<T> = ::std::result::Result<T, Diagnostic>;

struct Reader<'a> {
	source: &'a Arr<u8>,
	iter: Iter<'a, u8>,
	peek: Ascii,
	pos_cell: Cell<Pos>,
}
impl<'a> Reader<'a> {
	fn new(source: &'a Arr<u8>) -> Reader<'a> {
		let len = source.len();
		assert!(len >= 2);

		let slast = source[len - 1];
		assert!(Ascii(slast) == ascii::ZERO);
		let snextlast = source[len - 2];
		assert!(Ascii(snextlast) == ascii::NL);

		let mut iter = source.iter();
		let peek = Ascii(*iter.next().unwrap());
		Reader { source, iter, peek, pos_cell: Cell::new(POS_ZERO) }
	}

	fn pos(&self) -> Pos {
		self.pos_cell.get()
	}

	fn peek(&self) -> Ascii {
		self.peek
	}

	fn skip(&mut self) {
		let x = self.iter.next();
		self.peek = if let Some(ch) = x { Ascii(*ch) } else { ascii::ZERO }
	}

	fn skip2(&mut self) {
		self.skip();
		self.skip();
	}

	fn read_char(&mut self) -> Ascii {
		let res = self.peek;
		self.skip();
		res
	}

	fn slice_from(&self, start_pos: Pos) -> Arr<u8> {
		let a = start_pos.index as usize;
		let b = self.pos_cell.get().index as usize;
		self.source.copy_slice(a, b)
	}
}

pub struct Lexer<'a> {
	reader: Reader<'a>,
	indent: u32,
	dedenting: u32,
	token_value: Arr<u8>,
	diagnostic: Option<Diagnostic>,
}
impl<'a> Lexer<'a> {
	pub fn new(source: &'a Arr<u8>) -> Lexer {
		Lexer {
			reader: Reader::new(source),
			indent: 0,
			dedenting: 0,
			token_value: Arr::empty(),
			diagnostic: None,
		}
	}

	fn move_token_value(&mut self) -> Arr<u8> {
		replace(&mut self.token_value, Arr::empty())
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

	pub fn token_string(&mut self) -> Arr<u8> {
		self.move_token_value()
	}

	pub fn token_sym(&mut self) -> Sym {
		Sym::from_arr(self.move_token_value())
	}

	fn read_char(&mut self) -> Ascii {
		self.reader.read_char()
	}

	fn peek(&self) -> Ascii {
		self.reader.peek()
	}

	fn skip(&mut self) {
		self.reader.skip()
	}

	pub fn pos(&self) -> Pos {
		self.reader.pos()
	}

	fn slice_from(&self, pos: Pos) -> Arr<u8> {
		self.reader.slice_from(pos)
	}

	fn single_char_loc(&self) -> Loc {
		Loc::single_char(self.pos().decr())
	}

	pub fn loc_from(&self, start: Pos) -> Loc {
		Loc { start, end: self.pos() }
	}

	fn skip_while<F : Fn(Ascii) -> bool>(&mut self, pred: F) {
		while pred(self.peek()) {
			self.reader.skip();
		}
	}

	fn skip_empty_lines(&mut self) {
		self.skip_while(|ch| ch == ascii::NL)
	}

	fn next_quote_part(&mut self) -> QuoteEnd {
		let mut b = ArrBuilder::<u8>::new();
		let mut is_end = false;
		loop {
			match self.read_char().0 {
				ascii::U8_DOUBLE_QUOTE => {
					is_end = true;
					break;
				}
				ascii::U8_CURLYL => {
					is_end = false;
					break;
				}
				ascii::U8_NL =>
					panic!(),
				ascii::U8_BACKSLASH => {
					let ch = self.read_char();
					b.add(escape(ch).0);
					break;
				}
				ch => {
					b.add(ch);
					break;
				}
			}
		}
		self.token_value = b.finish();
		if is_end { QuoteEnd::QuoteEnd } else { QuoteEnd::QuoteInterpolation }
	}

	fn take_number(&mut self, start_pos: Pos, is_signed: bool) -> Token {
		self.skip_while(Ascii::is_digit);
		let is_float = self.peek() == ascii::DOT;
		if is_float {
			self.skip();
			if !self.peek().is_digit() {
				panic!()
			}
			self.skip_while(Ascii::is_digit);
		}
		self.token_value = self.slice_from(start_pos);
		if is_float { Token::FloatLiteral } else if is_signed { Token::IntLiteral } else { Token::NatLiteral }
	}

	fn take_name_or_keyword(&mut self, start_pos: Pos) -> Token {
		self.skip_while(Ascii::is_name_char);
		let s = self.slice_from(start_pos);

		if let Some(kw) = Token::keyword_from_name(&s) {
			return kw
		}

		self.token_value = s;
		return Token::Name;
	}

	fn take_string_like<F : Fn(Ascii) -> bool>(&mut self, start_pos: Pos, pred: F) {
		self.skip_while(pred);
		self.token_value = self.slice_from(start_pos);
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
		let ch = self.read_char();
		match ch.0 {
			ascii::U8_0 => {
				// Remember to dedent before finishing
				if self.indent != 0 {
					self.indent -= 1;
					self.dedenting = self.indent;
					Token::Dedent
				} else {
					Token::EOF
				}
			}

			ascii::U8_SPACE => {
				if self.peek() == ascii::NL {
					self.diagnostic = Some(Diagnostic(self.single_char_loc(), DiagnosticData::TrailingSpace));
					Token::Diagnostic
				} else {
					Token::Space
				}
			}

			ascii::U8_BAR => panic!(),

			ascii::U8_BACKSLASH => Token::Backslash,
			ascii::U8_COMMA => Token::Comma,
			ascii::U8_COLON => Token::Colon,
			ascii::U8_PARENL => Token::ParenL,
			ascii::U8_PARENR => Token::ParenR,
			ascii::U8_BRACKETL => Token::BracketL,
			ascii::U8_BRACKETR => Token::BracketR,
			ascii::U8_CURLYL => Token::CurlyL,
			ascii::U8_CURLYR => Token::CurlyR,
			ascii::U8_UNDERSCORE => Token::Underscore,
			ascii::U8_DOT => Token::Dot,

			ascii::U8_DOUBLE_QUOTE => {
				let qp = self.next_quote_part();
				if qp == QuoteEnd::QuoteEnd { Token::StringLiteral } else { Token::QuoteStart }
			}

			ascii::U8_DIGIT_0 ... ascii::U8_DIGIT_9_PLUS_ONE =>
				self.take_number(start, /*isSigned*/ false),

			ascii::U8_LOWER_A ... ascii::U8_LOWER_Z_PLUS_ONE =>
				self.take_name_or_keyword(start),

			ascii::U8_UPPER_A ... ascii::U8_UPPER_Z_PLUS_ONE => {
				self.take_string_like(start, Ascii::is_name_char);
				Token::Operator
			}

			ascii::U8_MINUS | ascii::U8_PLUS =>
				if self.peek().is_digit() {
					self.take_number(start, /*isSigned*/ true)
				} else {
					self.take_string_like(start, Ascii::is_operator_char);
					Token::Operator
				},

			ascii::U8_TIMES | ascii::U8_SLASH | ascii::U8_CARET | ascii::U8_QUESTION | ascii::U8_LESS | ascii::U8_GREATER => {
				self.take_string_like(start, Ascii::is_operator_char);
				Token::Operator
			}

			ch => {
				self.diagnostic = Some(Diagnostic(self.single_char_loc(), DiagnosticData::UnrecognizedCharacter(ch as char)));
				Token::Diagnostic
			}

		}
	}

	fn try_take(&mut self, ch: Ascii) -> bool {
		let res = self.peek() == ch;
		if res { self.skip() }
		true
	}

	pub fn expect_newline_character(&mut self) -> Result<()> {
		self.expect_character(ascii::NL, "newline")
	}

	pub fn expect_tab_character(&mut self) -> Result<()> {
		self.expect_character(ascii::TAB, "tab")
	}

	fn expect_character(&mut self, expected: Ascii, expected_desc: &'static str) -> Result<()> {
		let actual = self.read_char();
		if actual == expected {
			Ok(())
		} else {
			Err(Diagnostic(self.single_char_loc(), DiagnosticData::UnexpectedCharacter(actual.to_char(), expected_desc)))
		}
	}

	fn expect_character_by_predicate<F : Fn(Ascii) -> bool>(&mut self, pred: F, expected_desc: &'static str) -> Result<()> {
		let actual = self.read_char();
		if pred(actual) {
			Ok(())
		} else {
			Err(Diagnostic(self.single_char_loc(), DiagnosticData::UnexpectedCharacter(actual.to_char(), expected_desc)))
		}
	}

	fn lex_indent(&mut self) -> Result<u32> {
		let start = self.pos();
		self.skip_while(|ch| ch == ascii::TAB);
		let count = self.pos() - start;
		if self.peek() == ascii::SPACE {
			Err(Diagnostic(self.loc_from(start), DiagnosticData::LeadingSpace))
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
				Err(Diagnostic(self.single_char_loc(), DiagnosticData::TooMuchIndent(old_indent, new_indent)))
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

		Ok(if self.try_take(ascii::TAB) {
			NewlineOrDedent::Newline
		} else {
			self.indent -= 1;
			NewlineOrDedent::Dedent
		})
	}

	pub fn take_newline_or_indent(&mut self) -> Result<NewlineOrIndent> {
		self.expect_newline_character()?;
		self.skip_empty_lines();
		for _ in 0..self.indent {
			self.expect_tab_character()?
		}

		Ok(if self.try_take(ascii::TAB) {
			self.indent += 1;
			NewlineOrIndent::Indent
		} else {
			NewlineOrIndent::Newline
		})
	}

	pub fn at_eof(&self) -> bool {
		self.peek() == ascii::ZERO
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
		if !self.try_take(ascii::NL) {
			return Ok(false)
		}

		let x = self.handle_newline()?;
		if x == Token::Dedent {
			Ok(true)
		} else {
			Err(self.unexpected_token(start, x, "dedent"))
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
		if !self.try_take(ascii::NL) {
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
		Ok(self.try_take(ascii::TAB))
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
		self.try_take(ascii::SPACE)
	}

	pub fn take_equals(&mut self) -> Result<()> { self.expect_character(ascii::EQUAL, "'='") }
	pub fn take_space(&mut self) -> Result<()> { self.expect_character(ascii::SPACE, "space") }
	pub fn take_parenl(&mut self) -> Result<()> { self.expect_character(ascii::PARENL, "'('") }
	pub fn take_parenr(&mut self) -> Result<()> { self.expect_character(ascii::PARENR, "')'") }
	pub fn take_bracketl(&mut self) -> Result<()> { self.expect_character(ascii::BRACKETL, "'['") }
	pub fn take_bracketr(&mut self) -> Result<()> { self.expect_character(ascii::BRACKETR, "']'") }
	pub fn take_comma(&mut self) -> Result<()> { self.expect_character(ascii::COMMA, "','") }
	pub fn take_dot(&mut self) -> Result<()> { self.expect_character(ascii::DOT, "'.'") }

	pub fn try_take_equals(&mut self) -> bool { self.try_take(ascii::EQUAL) }
	pub fn try_take_parenr(&mut self) -> bool { self.try_take(ascii::PARENR) }
	pub fn try_take_dot(&mut self) -> bool { self.try_take(ascii::DOT) }
	pub fn try_take_colon(&mut self) -> bool { self.try_take(ascii::COLON) }
	pub fn try_take_bracketl(&mut self) -> bool { self.try_take(ascii::BRACKETL) }
	pub fn try_take_bracketr(&mut self) -> bool { self.try_take(ascii::BRACKETR) }

	pub fn take_specific_keyword(&mut self, kw: &'static str) -> Result<()> {
		self.must_read(kw, kw)
	}

	pub fn take_ty_name_string(&mut self) -> Result<Arr<u8>> {
		let start_pos = self.pos();
		self.expect_character_by_predicate(Ascii::is_upper_case_letter, "type name")?;
		self.skip_while(Ascii::is_name_char);
		Ok(self.slice_from(start_pos))
	}

	pub fn take_name_string(&mut self) -> Result<Arr<u8>> {
		let start_pos = self.pos();
		self.expect_character_by_predicate(Ascii::is_lower_case_letter, "(non-type) name")?;
		self.skip_while(Ascii::is_name_char);
		Ok(self.slice_from(start_pos))
	}

	pub fn take_name(&mut self) -> Result<Sym> {
		let v = self.take_ty_name_string()?;
		Ok(Sym::from_arr(v))
	}
	pub fn take_ty_name(&mut self) -> Result<Sym> {
		let v = self.take_name_string()?;
		Ok(Sym::from_arr(v))
	}

	pub fn unexpected_token(&self, start_pos: Pos, actual: Token, expected_desc: &'static str) -> Diagnostic {
		self.unexpected(start_pos, expected_desc, actual.token_name())
	}

	pub fn unexpected(&self, start_pos: Pos, actual_desc: &'static str, expected_desc: &'static str, ) -> Diagnostic {
		Diagnostic(self.loc_from(start_pos), DiagnosticData::UnexpectedToken(expected_desc, actual_desc))
	}

	pub fn take_catch_or_finally(&mut self) -> Result<CatchOrFinally> {
		match self.read_char() {
			ascii::C => {
				self.must_read("atch", "catch")?;
				Ok(CatchOrFinally::Catch)
			}
			ascii::F => {
				self.must_read("inally", "finally")?;
				Ok(CatchOrFinally::Finally)
			}
			ch =>
				Err(self.unexpected_char(ch, "'catch' or 'finally'"))
		}
	}

	pub fn take_slot_keyword(&mut self) -> Result<SlotKw> {
		self.must_read("va", "'val' or 'var'")?;
		match self.read_char() {
			ascii::L =>
				Ok(SlotKw::Val),
			ascii::R =>
				Ok(SlotKw::Var),
			ch =>
				Err(self.unexpected_char(ch, "'val' or 'var'"))
		}
	}

	pub fn take_method_keyword_or_eof(&mut self) -> Result<MethodKw> {
		if self.at_eof() {
			return Ok(MethodKw::Eof)
		}

		match self.read_char() {
			ascii::D => {
				self.must_read("ef", "def")?;
				Ok(MethodKw::Def)
			}
			ascii::F => {
				self.must_read("un", "fun")?;
				Ok(MethodKw::Fun)
			}
			ascii::I => {
				self.must_read_char(ascii::S, "is")?;
				Ok(MethodKw::Is)
			}
			ch =>
				Err(self.unexpected_char(ch, "'def' or 'fun' or 'is'"))
		}
	}

	fn must_read(&mut self, must_read_me: &'static str, expected_desc: &'static str) -> Result<()> {
		for byte in must_read_me.as_bytes() {
			self.must_read_char(Ascii(*byte), expected_desc)?
		}
		Ok(())
	}

	fn must_read_char(&mut self, must_read_me: Ascii, expected_desc: &'static str) -> Result<()> {
		let ch = self.read_char();
		if ch == must_read_me {
			Ok(())
		} else {
			Err(self.unexpected_char(ch, expected_desc))
		}
	}

	fn unexpected_char(&self, actual: Ascii, expected_desc: &'static str) -> Diagnostic {
		Diagnostic(self.single_char_loc(), DiagnosticData::UnexpectedCharacter(actual.to_char(), expected_desc))
	}

}

#[derive(Eq, PartialEq)]
pub enum MethodKw { Def, Fun, Is, Eof }
pub enum SlotKw { Val, Var }
pub enum CatchOrFinally { Catch, Finally }

pub enum NewlineOrIndent { Newline, Indent }
pub enum NewlineOrDedent { Newline, Dedent }

fn escape(escaped: Ascii) -> Ascii {
	match escaped.0 {
		ascii::U8_DOUBLE_QUOTE | ascii::U8_CURLYL => escaped,
		ascii::U8_N => ascii::NL,
		ascii::U8_T => ascii::TAB,
		_ => panic!() // bad escape
	}
}

#[derive(Eq, PartialEq)]
pub enum QuoteEnd { QuoteEnd, QuoteInterpolation }
