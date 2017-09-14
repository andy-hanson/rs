use util::arena::NoDrop;
use util::string_maker::{Show, Shower};

pub enum ParseDiag {
	TooMuchIndent { old: u32, new: u32 },
	LeadingSpace,
	TrailingSpace,
	EmptyExpression,
	BlockCantEndInLet,
	PrecedingEquals,
	UnrecognizedCharacter(char),
	UnexpectedCharacterType { actual: u8, expected_desc: &'static [u8] },
	UnexpectedCharacter { actual: u8, expected: u8 },
	UnexpectedToken { expected: &'static [u8], actual: &'static [u8] },
}
impl NoDrop for ParseDiag {}
impl<'a> Show for &'a ParseDiag {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			ParseDiag::TooMuchIndent { old, new } => {
				s.add("Expected only ")?
					.add(old + 1)?
					.add(" indents; actual: ")?
					.add(new)?;
			}
			ParseDiag::LeadingSpace => {
				s.add("Line begins with a space. (Use tabs to indent.)")?;
			}
			ParseDiag::TrailingSpace => {
				s.add("Line ends in a space.")?;
			}
			ParseDiag::EmptyExpression => {
				s.add("Expression has no content.")?;
			}
			ParseDiag::BlockCantEndInLet => {
				s.add("`let` may not be the last line in a block.")?;
			}
			ParseDiag::PrecedingEquals => {
				s.add("Unusual expression preceding `=`.")?; //TODO: better error message
			}
			ParseDiag::UnrecognizedCharacter(ch) => {
				s.add("Illegal character '")?.add(ch)?.add("'.")?;
			}
			ParseDiag::UnexpectedCharacterType { actual, expected_desc } => {
				s.add("Unexpected character '")?;
				show_char(actual, s)?;
				s.add("'; expected: ")?.add(expected_desc)?;
			}
			ParseDiag::UnexpectedCharacter { actual, expected } => {
				s.add("Unexpected character '")?;
				show_char(actual, s)?;
				s.add("'; expected: ")?;
				show_char(expected, s)?;
			}
			ParseDiag::UnexpectedToken { expected, actual } => {
				s.add("Expected token type '")?
					.add(expected)?
					.add("', got: '")?
					.add(actual)?
					.add("'.")?;
			}
		}
		Ok(())
	}
}

fn show_char<S: Shower>(ch: u8, s: &mut S) -> Result<(), S::Error> {
	match ch {
		b'\t' => {
			s.add("tab")?;
		}
		b' ' => {
			s.add("space")?;
		}
		b'\n' => {
			s.add("newline")?;
		}
		_ => {
			s.add(char::from(ch))?;
		}
	}
	Ok(())
}
