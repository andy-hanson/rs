use util::dict::Dict;

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
	pub fn token_name(self) -> &'static [u8] {
		match TOKEN_TO_NAME.get(self) {
			Some(name) => name,
			None =>
				match self {
					Token::Diagnostic => unreachable!(), // Should be handled by the parser.
					Token::Name => b"name",
					Token::TyName => b"type name",
					Token::Operator => b"operator",
					Token::NatLiteral => b"Nat literal",
					Token::IntLiteral => b"Int literal",
					Token::FloatLiteral => b"Float literal",
					Token::StringLiteral => b"String literal",
					Token::QuoteStart => b"Quote start",
					Token::Backslash => b"\\",
					Token::BracketL => b"[",
					Token::BracketR => b"]",
					Token::Colon => b":",
					Token::Comma => b",",
					Token::CurlyL => b"{",
					Token::CurlyR => b"}",
					Token::Dedent => b"dedent",
					Token::Dot => b".",
					Token::EOF => b"EOF",
					Token::Equals => b"=",
					Token::Indent => b"indent",
					Token::Newline => b"newline",
					Token::ParenL => b"(",
					Token::ParenR => b")",
					Token::Space => b" ",
					Token::Underscore => b"_",
					_ => panic!(), // Should be handled by TOKEN_TO_NAME
				},
		}
	}
}

lazy_static! {
	// TODO: use a static map? https://github.com/cbreeden/static-map
	static ref NAME_TO_TOKEN: Dict<&'static [u8], Token> = dict!(
		b"abstract" => Token::Abstract,
		b"array" => Token::Array,
		b"assert" => Token::Assert,
		b"builtin" => Token::Builtin,
		b"catch" => Token::Catch,
		b"def" => Token::Def,
		b"do" => Token::Do,
		b"enum" => Token::Enum,
		b"else" => Token::Else,
		b"false" => Token::False,
		b"finally" => Token::Finally,
		b"for" => Token::For,
		b"fun" => Token::Fun,
		b"generic" => Token::Generic,
		b"get" => Token::Get,
		b"if" => Token::If,
		b"io" => Token::Io,
		b"import" => Token::Import,
		b"in" => Token::In,
		b"is" => Token::Is,
		b"new" => Token::New,
		b"recur" => Token::Recur,
		b"self" => Token::SelfKw,
		b"set" => Token::Set,
		b"slots" => Token::Slots,
		b"then" => Token::Then,
		b"true" => Token::True,
		b"try" => Token::Try,
		b"val" => Token::Val,
		b"var" => Token::Var,
		b"when" => Token::When,
	);

	static ref TOKEN_TO_NAME: Dict<Token, &'static [u8]> =
		Dict::from_iterator(NAME_TO_TOKEN.iter().map(|(k, v)|
			(*v, *k)));
}

impl Token {
	pub fn keyword_from_name(name: &[u8]) -> Option<Self> {
		NAME_TO_TOKEN.get(name).cloned()
	}
}
