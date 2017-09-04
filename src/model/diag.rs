use util::arr::Arr;
use util::loc::Loc;
use util::path::{Path, RelPath};
use util::ptr::Ptr;
use util::string_maker::{Show, Shower};
use util::sym::Sym;

use super::super::model::class::{ClassDeclaration, MemberDeclaration, SlotDeclaration};
use super::super::model::effect::Effect;
use super::super::model::expr::Local;
use super::super::model::method::{AbstractMethod, MethodOrAbstract, MethodWithBody, Parameter};
use super::super::model::ty::Ty;

pub struct Diagnostic(pub Loc, pub Diag);

pub enum Diag {
	CircularDependency(Path, RelPath),
	CantFindLocalModule(Path, RelPath),

	TooMuchIndent(/*expected*/ u32, /*actual*/ u32),
	LeadingSpace,
	TrailingSpace,
	EmptyExpression,
	BlockCantEndInLet,
	PrecedingEquals,
	UnrecognizedCharacter(char),
	UnexpectedCharacter(char, &'static [u8]),
	UnexpectedToken { expected: &'static [u8], actual: &'static [u8] },

	CantCombineTypes(Ty, Ty),
	NotAssignable { expected: Ty, actual: Ty },
	MemberNotFound(Ptr<ClassDeclaration>, Sym),
	CantAccessSlotFromStaticMethod(Ptr<SlotDeclaration>),
	MissingEffectToGetSlot(Ptr<SlotDeclaration>),
	MissingEffectToSetSlot(/*actual*/ Effect, Ptr<SlotDeclaration>),
	DelegatesNotYetSupported,
	CantAccessStaticMethodThroughInstance(Ptr<MethodWithBody>),
	IllegalEffect { allowed: Effect, required: Effect },
	ArgumentCountMismatch(MethodOrAbstract, usize),
	ClassNotFound(Sym),
	StaticMethodNotFound(Ptr<ClassDeclaration>, Sym),
	CantCallInstanceMethodFromStaticMethod(MethodOrAbstract),
	NotATailCall,
	NewInvalid(Ptr<ClassDeclaration>),
	NewArgumentCountMismatch { actual: usize, expected: usize },
	CantSetNonSlot(MemberDeclaration),
	SlotNotMutable(Ptr<SlotDeclaration>),
	CantReassignParameter(Ptr<Parameter>),
	CantReassignLocal(Ptr<Local>),

	//mv
	NotAnAbstractClass(Ptr<ClassDeclaration>),
	ImplsMismatch { expected_names: Arr<Sym> },
	WrongImplParameters(Ptr<AbstractMethod>),
}
impl Show for Diag {
	fn show<S: Shower>(&self, s: &mut S) {
		let _ = s;
		match *self {
			Diag::CircularDependency(_, _) => unimplemented!(),
			Diag::CantFindLocalModule(_, _) => unimplemented!(),

			Diag::TooMuchIndent(_, _) => unimplemented!(),
			Diag::LeadingSpace => unimplemented!(),
			Diag::TrailingSpace => unimplemented!(),
			Diag::EmptyExpression => unimplemented!(),
			Diag::BlockCantEndInLet => unimplemented!(),
			Diag::PrecedingEquals => unimplemented!(),
			Diag::UnrecognizedCharacter(_) => unimplemented!(),
			Diag::UnexpectedCharacter(_, _) => unimplemented!(),
			Diag::UnexpectedToken { expected, actual } => {
				unused!(expected, actual);
				unimplemented!()
			}
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
			Diag::ImplsMismatch { ref expected_names } => {
				unused!(expected_names);
				unimplemented!()
			}
			Diag::WrongImplParameters(_) => unimplemented!(),
		}
	}
}
