use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;

use util::arr::Arr;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Token {
	// Indicates that the lexer has a diagnostic, so the parser should just return that.
	Diagnostic,

	// Lexer will also write to token_value when returning these symbols
	Name,
	TyName,
	Operator,
	NatLiteral,
	IntLiteral,
	FloatLiteral,
	StringLiteral,
	QuoteStart,

	// Keywords
	Abstract,
	Array,
	Assert,
	Builtin,
	Catch,
	Def,
	Do,
	Enum,
	Else,
	False,
	Finally,
	For,
	Fun,
	Generic,
	Get,
	If,
	Io,
	Import,
	In,
	Is,
	New,
	Pass,
	Recur,
	SelfKw,
	Set,
	Slots,
	Then,
	True,
	Try,
	Val,
	Var,
	When,

	// Punctuation
	Backslash,
	BracketL,
	BracketR,
	Colon,
	ColonEquals,
	Comma,
	CurlyL,
	CurlyR,
	Dedent,
	Dot,
	EOF,
	Equals,
	Indent,
	Newline,
	ParenL,
	ParenR,
	Space,
	Underscore,
}
impl Token {
	pub fn token_name(self) -> &'static str {
		match TOKEN_TO_NAME.get(&self) {
			Some(name) => name,
			None => match self {
				Token::Diagnostic => panic!(),
				Token::Name => "name",
				Token::TyName => "type name",
				Token::Operator => "operator",
				Token::NatLiteral => "Nat literal",
				Token::IntLiteral => "Int literal",
				Token::FloatLiteral => "Float literal",
				Token::StringLiteral => "String literal",
				Token::QuoteStart => "Quote start",
				Token::Backslash => "\\",
				Token::BracketL => "[",
				Token::BracketR => "]",
				Token::Colon => ":",
				Token::ColonEquals => ":=",
				Token::Comma => ",",
				Token::CurlyL => "{",
				Token::CurlyR => "}",
				Token::Dedent => "dedent",
				Token::Dot => ".",
				Token::EOF => "EOF",
				Token::Equals => "=",
				Token::Indent => "indent",
				Token::Newline => "newline",
				Token::ParenL => "(",
				Token::ParenR => ")",
				Token::Space => " ",
				Token::Underscore => "_",
				_ => panic!(), // Should be handled by TOKEN_TO_NAME
			},
		}
	}
}

fn must_add<K: Hash + Eq, V>(h: &mut HashMap<K, V>, k: K, v: V) {
	if let Some(_old) = h.insert(k, v) {
		panic!()
	}
}

lazy_static! {
    static ref NAME_TO_TOKEN: HashMap<Box<[u8]>, Token> = {
let mut h = HashMap::<Box<[u8]>, Token>::new();
assoc(&mut h, "abstract", Token::Abstract);
assoc(&mut h, "array", Token::Array);
assoc(&mut h, "assert", Token::Assert);
assoc(&mut h, "builtin", Token::Builtin);
assoc(&mut h, "catch", Token::Catch);
assoc(&mut h, "def", Token::Def);
assoc(&mut h, "do", Token::Do);
assoc(&mut h, "enum", Token::Enum);
assoc(&mut h, "else", Token::Else);
assoc(&mut h, "false", Token::False);
assoc(&mut h, "finally", Token::Finally);
assoc(&mut h, "for", Token::For);
assoc(&mut h, "fun", Token::Fun);
assoc(&mut h, "generic", Token::Generic);
assoc(&mut h, "get", Token::Get);
assoc(&mut h, "if", Token::If);
assoc(&mut h, "io", Token::Io);
assoc(&mut h, "import", Token::Import);
assoc(&mut h, "in", Token::In);
assoc(&mut h, "is", Token::Is);
assoc(&mut h, "new", Token::New);
assoc(&mut h, "pass", Token::Pass);
assoc(&mut h, "recur", Token::Recur);
assoc(&mut h, "self", Token::SelfKw);
assoc(&mut h, "set", Token::Set);
assoc(&mut h, "slots", Token::Slots);
assoc(&mut h, "then", Token::Then);
assoc(&mut h, "true", Token::True);
assoc(&mut h, "try", Token::Try);
assoc(&mut h, "val", Token::Val);
assoc(&mut h, "var", Token::Var);
assoc(&mut h, "when", Token::When);
h
};
static ref TOKEN_TO_NAME: HashMap<Token, String> = {
let i = NAME_TO_TOKEN.iter().map(|(k, v)|
(*v, String::from_utf8(k.clone().into_vec()).unwrap()));
HashMap::<Token, String>::from_iter(i)
};
}

fn assoc(h: &mut HashMap<Box<[u8]>, Token>, s: &str, t: Token) {
	must_add(h, Arr::copy_from_str(s).into_box(), t)
}

impl Token {
	pub fn keyword_from_name(name: &[u8]) -> Option<Token> {
		NAME_TO_TOKEN.get(name).cloned()
	}
}
