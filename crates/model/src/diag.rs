use util::arena::{NoDrop, Up};
use util::loc::{LineAndColumnGetter, Loc};
use util::path::{Path, RelPath};
use util::string_maker::{Show, Shower};
use util::sym::Sym;

use super::class::{ClassDeclaration, MemberDeclaration, SlotDeclaration};
use super::effect::Effect;
use super::expr::Local;
use super::method::{AbstractMethod, MethodOrAbstract, MethodWithBody, Parameter};
use super::module::ModuleOrFail;
use super::ty::Ty;

pub struct Diagnostic<'a>(pub Loc, pub Diag<'a>);
impl<'a> NoDrop for Diagnostic<'a> {}

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
	fn show<S : Shower>(self, s: &mut S) {
		match *self {
			ParseDiag::TooMuchIndent { old, new } => {
				s.add("Expected only ").add(old + 1).add(" indents; actual: ").add(new);
			}
			ParseDiag::LeadingSpace => {
				s.add("Line begins with a space. (Use tabs to indent.)");
			}
			ParseDiag::TrailingSpace => {
				s.add("Line ends in a space.");
			}
			ParseDiag::EmptyExpression => {
				s.add("Expression has no content.");
			}
			ParseDiag::BlockCantEndInLet => {
				s.add("`let` may not be the last line in a block.");
			}
			ParseDiag::PrecedingEquals => {
				s.add("Unusual expression preceding `=`."); //TODO: better error message
			}
			ParseDiag::UnrecognizedCharacter(ch) => {
				s.add("Illegal character '").add(ch).add("'.");
			}
			ParseDiag::UnexpectedCharacterType { actual, expected_desc } => {
				s.add("Unexpected character '");
				show_char(actual, s);
				s.add("'; expected: ").add(expected_desc);
			}
			ParseDiag::UnexpectedCharacter { actual, expected } => {
				s.add("Unexpected character '");
				show_char(actual, s);
				s.add("'; expected: ");
				show_char(expected, s);
			}
			ParseDiag::UnexpectedToken { expected, actual } => {
				s.add("Expected token type '").add(expected).add("', got: '").add(actual).add("'.");
			}
		}
	}
}

fn show_char<S : Shower>(ch: u8, s: &mut S) {
	match ch {
		b'\t' => {
			s.add("tab");
		}
		b' ' => {
			s.add("space");
		}
		b'\n' => {
			s.add("newline");
		}
		_ => {
			s.add(char::from(ch));
		}
	}
}

pub enum Diag<'a> {
	//TODO: Diag must be NoDrop, but this will leak the path!
	CircularDependency(Path<'a>, RelPath<'a>),
	CantFindLocalModule(Path<'a>, RelPath<'a>),
	ParseError(ParseDiag),

	CantCombineTypes(Ty<'a>, Ty<'a>),
	NotAssignable { expected: Ty<'a>, actual: Ty<'a> },
	MemberNotFound(Up<'a, ClassDeclaration<'a>>, Sym),
	CantAccessSlotFromStaticMethod(Up<'a, SlotDeclaration<'a>>),
	MissingEffectToGetSlot(Up<'a, SlotDeclaration<'a>>),
	MissingEffectToSetSlot(/*actual*/ Effect, Up<'a, SlotDeclaration<'a>>),
	DelegatesNotYetSupported,
	CantAccessStaticMethodThroughInstance(Up<'a, MethodWithBody<'a>>),
	IllegalEffect { allowed: Effect, required: Effect },
	ArgumentCountMismatch(MethodOrAbstract<'a>, usize),
	ClassNotFound(Sym),
	StaticMethodNotFound(Up<'a, ClassDeclaration<'a>>, Sym),
	CantCallInstanceMethodFromStaticMethod(MethodOrAbstract<'a>),
	NotATailCall,
	NewInvalid(Up<'a, ClassDeclaration<'a>>),
	NewArgumentCountMismatch { actual: usize, expected: usize },
	CantSetNonSlot(MemberDeclaration<'a>),
	SlotNotMutable(Up<'a, SlotDeclaration<'a>>),
	CantReassignParameter(Up<'a, Parameter<'a>>),
	CantReassignLocal(Up<'a, Local<'a>>),

	//mv
	NotAnAbstractClass(Up<'a, ClassDeclaration<'a>>),
	ImplsMismatch { expected_names: &'a [Sym] },
	WrongImplParameters(Up<'a, AbstractMethod<'a>>),
}
impl<'a> NoDrop for Diag<'a> {}
impl<'d, 'a> Show for &'d Diag<'a> {
	fn show<S: Shower>(self, s: &mut S) {
		match *self {
			Diag::CircularDependency(_, _) => unimplemented!(),
			Diag::CantFindLocalModule(_, _) => unimplemented!(),
			Diag::ParseError(ref p) => p.show(s),

			Diag::CantCombineTypes(_, _) => unimplemented!(),
			Diag::NotAssignable { ref expected, ref actual } => {
				unused!(expected, actual);
				unimplemented!()
			}
			Diag::MemberNotFound(_, _) => unimplemented!(),
			Diag::CantAccessSlotFromStaticMethod(_) => unimplemented!(),
			Diag::MissingEffectToGetSlot(_) => unimplemented!(),
			Diag::MissingEffectToSetSlot(_, _) => unimplemented!(),
			Diag::DelegatesNotYetSupported => unimplemented!(),
			Diag::CantAccessStaticMethodThroughInstance(_) => unimplemented!(),
			Diag::IllegalEffect { allowed, required } => {
				unused!(allowed, required);
				unimplemented!()
			}
			Diag::ArgumentCountMismatch(_, _) => unimplemented!(),
			Diag::ClassNotFound(_) => unimplemented!(),
			Diag::StaticMethodNotFound(_, _) => unimplemented!(),
			Diag::CantCallInstanceMethodFromStaticMethod(_) => unimplemented!(),
			Diag::NotATailCall => unimplemented!(),
			Diag::NewInvalid(_) => unimplemented!(),
			Diag::NewArgumentCountMismatch { actual, expected } => {
				unused!(actual, expected);
				unimplemented!()
			}
			Diag::CantSetNonSlot(_) => unimplemented!(),
			Diag::SlotNotMutable(_) => unimplemented!(),
			Diag::CantReassignParameter(_) => unimplemented!(),
			Diag::CantReassignLocal(_) => unimplemented!(),
			Diag::NotAnAbstractClass(_) => unimplemented!(),
			Diag::ImplsMismatch { expected_names } => {
				unused!(expected_names);
				unimplemented!()
			}
			Diag::WrongImplParameters(_) => unimplemented!(),
		}
	}
}

pub fn show_diagnostics<'a, S : Shower>(module: &ModuleOrFail<'a>, s: &mut S) {
	let diags = module.diagnostics();
	assert!(diags.any()); // Else don't call this.

	let source = module.source();
	let text = source.text();
	let lc = LineAndColumnGetter::new(text);
	for &Diagnostic(loc, ref data) in diags.iter() {
		let lc_loc = lc.line_and_column_at_loc(loc);
		source.show(s).add(&lc_loc).add(": ").add(data);
	}
}
