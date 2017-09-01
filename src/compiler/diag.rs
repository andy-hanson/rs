use util::arr::Arr;
use util::loc::Loc;
use util::path::{Path, RelPath};
use util::ptr::Ptr;
use util::sym::Sym;

use super::model::class::{ClassDeclaration, MemberDeclaration, SlotDeclaration};
use super::model::effect::Effect;
use super::model::expr::Local;
use super::model::method::{AbstractMethod, MethodOrAbstract, MethodWithBody, Parameter};
use super::model::ty::Ty;

pub struct Diagnostic(pub Loc, pub Diag);

pub enum Diag {
	CircularDependency(Path, RelPath),
	CantFindLocalModule(Path, RelPath),

	TooMuchIndent(/*expected*/ u32, /*actual*/ u32),
	LeadingSpace,
	TrailingSpace,
	ExmptyExpression,
	BlockCantEndInLet,
	PrecedingEquals,
	UnrecognizedCharacter(char),
	UnexpectedCharacter(char, &'static str),
	UnexpectedToken(&'static str, &'static str),

	CantCombineTypes(Ty, Ty),
	NotAssignable(/*expected*/ Ty, /*actual*/ Ty),
	MemberNotFound(Ptr<ClassDeclaration>, Sym),
	CantAccessSlotFromStaticMethod(Ptr<SlotDeclaration>),
	MissingEffectToGetSlot(Ptr<SlotDeclaration>),
	MissingEffectToSetSlot(/*actual*/ Effect, Ptr<SlotDeclaration>),
	DelegatesNotYetSupported,
	CantAccessStaticMethodThroughInstance(Ptr<MethodWithBody>),
	IllegalEffect(/*allowed*/ Effect, /*actual*/ Effect),
	ArgumentCountMismatch(MethodOrAbstract, usize),
	ClassNotFound(Sym),
	StaticMethodNotFound(Ptr<ClassDeclaration>, Sym),
	CantCallInstanceMethodFromStaticMethod(MethodOrAbstract),
	NotATailCall,
	NewInvalid(Ptr<ClassDeclaration>),
	NewArgumentCountMismatch(/*actual*/ usize, /*expected*/ usize),
	CantSetNonSlot(MemberDeclaration),
	SlotNotMutable(Ptr<SlotDeclaration>),
	CantReassignParameter(Ptr<Parameter>),
	CantReassignLocal(Ptr<Local>),

	//mv
	NotAnAbstractClass(Ptr<ClassDeclaration>),
	ImplsMismatch { expected_names: Arr<Sym> },
	WrongImplParameters(Ptr<AbstractMethod>),
}
