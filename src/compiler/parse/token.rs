use util::arr::Arr;
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
			None =>
				match self {
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

lazy_static! {
// TODO: use a static map? https://github.com/cbreeden/static-map
	static ref NAME_TO_TOKEN: Dict<Arr<u8>, Token> = dict!(
		a("abstract") => Token::Abstract,
		a("array") => Token::Array,
		a("assert") => Token::Assert,
		a("builtin") => Token::Builtin,
		a("catch") => Token::Catch,
		a("def") => Token::Def,
		a("do") => Token::Do,
		a("enum") => Token::Enum,
		a("else") => Token::Else,
		a("false") => Token::False,
		a("finally") => Token::Finally,
		a("for") => Token::For,
		a("fun") => Token::Fun,
		a("generic") => Token::Generic,
		a("get") => Token::Get,
		a("if") => Token::If,
		a("io") => Token::Io,
		a("import") => Token::Import,
		a("in") => Token::In,
		a("is") => Token::Is,
		a("new") => Token::New,
		a("pass") => Token::Pass,
		a("recur") => Token::Recur,
		a("self") => Token::SelfKw,
		a("set") => Token::Set,
		a("slots") => Token::Slots,
		a("then") => Token::Then,
		a("true") => Token::True,
		a("try") => Token::Try,
		a("val") => Token::Val,
		a("var") => Token::Var,
		a("when") => Token::When,
	);

	static ref TOKEN_TO_NAME: Dict<Token, String> =
		Dict::from_iterator(NAME_TO_TOKEN.iter().map(|(k, v)|
			(*v, String::from_utf8(k.clone().into_vec()).unwrap())));
}

fn a(k: &'static str) -> Arr<u8> {
	Arr::copy_from_str(k)
}

impl Token {
	pub fn keyword_from_name(name: &[u8]) -> Option<Self> {
		NAME_TO_TOKEN.get(name).cloned()
	}
}
