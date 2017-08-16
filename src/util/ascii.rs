#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Ascii(pub u8);
impl Ascii {
	pub fn is_digit(self) -> bool {
		self.0 >= U8_DIGIT_0 && self.0 < U8_DIGIT_9_PLUS_ONE
	}

	pub fn is_name_char(self) -> bool {
		self.is_lower_case_letter() || self.is_upper_case_letter() || self.is_digit()
	}

	pub fn is_lower_case_letter(self) -> bool {
		self.0 >= U8_LOWER_A && self.0 < U8_LOWER_Z_PLUS_ONE
	}

	pub fn is_upper_case_letter(self) -> bool {
		self.0 >= U8_UPPER_A && self.0 < U8_UPPER_Z_PLUS_ONE
	}

	pub fn is_operator_char(self) -> bool {
		match self.0 {
			U8_MINUS | U8_PLUS | U8_TIMES | U8_SLASH | U8_CARET | U8_QUESTION | U8_LESS | U8_GREATER => true,
			_ => false,
		}
	}

	pub fn to_char(self) -> char {
		self.0 as char
	}
}

pub const U8_DIGIT_0: u8 = '0' as u8;
pub const U8_DIGIT_9_PLUS_ONE: u8 = '9' as u8 + 1; // Need +1 because rust uses exclusive ranges
pub const U8_LOWER_A: u8 = 'a' as u8;
pub const U8_LOWER_Z_PLUS_ONE: u8 = 'z' as u8 + 1;
pub const U8_UPPER_A: u8 = 'A' as u8;
pub const U8_UPPER_Z_PLUS_ONE: u8 = 'Z' as u8 + 1;

pub const C: Ascii = Ascii('c' as u8);
pub const D: Ascii = Ascii('d' as u8);
pub const F: Ascii = Ascii('f' as u8);
pub const I: Ascii = Ascii('i' as u8);
pub const L: Ascii = Ascii('l' as u8);
pub const R: Ascii = Ascii('r' as u8);
pub const S: Ascii = Ascii('s' as u8);
pub const COMMA: Ascii = Ascii(',' as u8);
pub const COLON: Ascii = Ascii(':' as u8);
pub const EQUAL: Ascii = Ascii('=' as u8);

pub const U8_0: u8 = 0;
pub const ZERO: Ascii = Ascii(U8_0);
pub const U8_NL: u8 = '\n' as u8;
pub const NL: Ascii = Ascii(U8_NL);
pub const U8_DOUBLE_QUOTE: u8 = '"' as u8;
pub const DOUBLE_QUOTE: Ascii = Ascii(U8_DOUBLE_QUOTE);
pub const U8_BACKSLASH: u8 = '\\' as u8;
pub const BACKSLASH: Ascii = Ascii(U8_BACKSLASH);
pub const U8_N: u8 = 'n' as u8;
pub const U8_T: u8 = 't' as u8;
pub const TAB: Ascii = Ascii('\t' as u8);
pub const U8_DOT: u8 = '.' as u8;
pub const DOT: Ascii = Ascii(U8_DOT);
pub const U8_SPACE: u8 = ' ' as u8;
pub const SPACE: Ascii = Ascii(U8_SPACE);
pub const U8_BAR: u8 = '|' as u8;
pub const U8_COMMA: u8 = ',' as u8;
pub const U8_COLON: u8 = ':' as u8;
pub const U8_PARENL: u8 = '(' as u8;
pub const PARENL: Ascii = Ascii(U8_PARENL);
pub const U8_PARENR: u8 = ')' as u8;
pub const PARENR: Ascii = Ascii(U8_PARENR);
pub const U8_BRACKETL: u8 = '[' as u8;
pub const BRACKETL: Ascii = Ascii(U8_BRACKETL);
pub const U8_BRACKETR: u8 = ']' as u8;
pub const BRACKETR: Ascii = Ascii(U8_BRACKETR);
pub const U8_CURLYL: u8 = '{' as u8;
pub const U8_CURLYR: u8 = '}' as u8;
pub const U8_UNDERSCORE: u8 = '_' as u8;

pub const U8_MINUS: u8 = '-' as u8;
pub const U8_PLUS: u8 = '+' as u8;
pub const U8_TIMES: u8 = '*' as u8;
pub const U8_SLASH: u8 = '/' as u8;
pub const U8_CARET: u8 = '^' as u8;
pub const U8_QUESTION: u8 = '?' as u8;
pub const U8_LESS: u8 = '<' as u8;
pub const U8_GREATER: u8 = '>' as u8;
