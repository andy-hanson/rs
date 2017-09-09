use util::arena::{NoDrop, Up};
use util::loc::Loc;
use util::path::{Path, RelPath};
use util::string_maker::{Show, Shower};
use util::sym::Sym;

use super::class::{ClassDeclaration, MemberDeclaration, SlotDeclaration};
use super::effect::Effect;
use super::expr::Local;
use super::method::{AbstractMethod, MethodOrAbstract, MethodWithBody, Parameter};
use super::ty::Ty;

pub struct Diagnostic<'a>(pub Loc, pub Diag<'a>);
impl<'a> NoDrop for Diagnostic<'a> {}

pub enum ParseDiag {
	TooMuchIndent(/*expected*/ u32, /*actual*/ u32),
	LeadingSpace,
	TrailingSpace,
	EmptyExpression,
	BlockCantEndInLet,
	PrecedingEquals,
	UnrecognizedCharacter(char),
	UnexpectedCharacter(char, &'static [u8]),
	UnexpectedToken { expected: &'static [u8], actual: &'static [u8] },
}
impl NoDrop for ParseDiag {}
impl<'a> Show for &'a ParseDiag {
	fn show<S : Shower>(self, s: &mut S) {
		match *self {
			ParseDiag::TooMuchIndent(_, _) => unimplemented!(),
			ParseDiag::LeadingSpace => unimplemented!(),
			ParseDiag::TrailingSpace => unimplemented!(),
			ParseDiag::EmptyExpression => unimplemented!(),
			ParseDiag::BlockCantEndInLet => unimplemented!(),
			ParseDiag::PrecedingEquals => unimplemented!(),
			ParseDiag::UnrecognizedCharacter(_) => unimplemented!(),
			ParseDiag::UnexpectedCharacter(_, _) => unimplemented!(),
			ParseDiag::UnexpectedToken { expected, actual } => {
				s.add("Expected token type '").add(expected).add("', got: '").add(actual).add("'.");
			}
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
