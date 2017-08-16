use util::loc::Loc;

pub struct Diagnostic(pub Loc, pub DiagnosticData);

pub enum DiagnosticData {
	TooMuchIndent(/*expected*/u32, /*actual*/u32),
	LeadingSpace,
	TrailingSpace,
	ExmptyExpression,
	BlockCantEndInLet,
	PrecedingEquals,
	UnrecognizedCharacter(char),
	UnexpectedCharacter(char, &'static str),
	UnexpectedToken(&'static str, &'static str),
}
