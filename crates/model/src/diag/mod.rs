use util::arena::NoDrop;
use util::iter::KnownLen;
use util::loc::{LineAndColumnGetter, Loc};
use util::path::{Path, RelPath};
use util::string_maker::{Show, Shower};
use util::sym::Sym;
use util::up::Up;

use super::class::{ClassDeclaration, MemberDeclaration, SlotDeclaration};
use super::effect::Effect;
use super::expr::Local;
use super::method::{AbstractMethod, MethodOrAbstract, MethodWithBody, Parameter};
use super::module::ModuleOrFail;
use super::ty::Ty;

mod parse_diag;
pub use self::parse_diag::ParseDiag;

pub struct Diagnostic<'a> {
	pub loc: Loc,
	pub diag: Diag<'a>,
}
impl<'a> NoDrop for Diagnostic<'a> {}


pub enum Diag<'a> {
	// Compile errors
	CircularDependency { from: Path<'a>, to: RelPath<'a> },
	CantFindLocalModule { from: Path<'a>, to: RelPath<'a> },
	ParseError(ParseDiag),

	// Checker errors
	CantCombineTypes(Ty<'a>, Ty<'a>),
	NotAssignable { expected: Ty<'a>, actual: Ty<'a> },
	MemberNotFound(Up<'a, ClassDeclaration<'a>>, Sym),
	CantAccessSlotFromStaticMethod(Up<'a, SlotDeclaration<'a>>),
	MissingEffectToGetSlot(Up<'a, SlotDeclaration<'a>>),
	MissingEffectToSetSlot { allowed_effect: Effect, slot: Up<'a, SlotDeclaration<'a>> },
	MethodUsedAsValue,
	CallsNonMethod,
	CantAccessStaticMethodThroughInstance(Up<'a, MethodWithBody<'a>>),
	IllegalSelfEffect { target_effect: Effect, method: MethodOrAbstract<'a> },
	ArgumentCountMismatch(MethodOrAbstract<'a>, usize),
	ClassNotFound(Sym),
	StaticMethodNotFound(Up<'a, ClassDeclaration<'a>>, Sym),
	CantCallInstanceMethodFromStaticMethod(MethodOrAbstract<'a>),
	NotATailCall,
	NewInvalid(Up<'a, ClassDeclaration<'a>>),
	NewArgumentCountMismatch { class: Up<'a, ClassDeclaration<'a>>, n_slots: usize, n_arguments: usize },
	CantSetNonSlot(MemberDeclaration<'a>),
	SlotNotMutable(Up<'a, SlotDeclaration<'a>>),
	CantReassignParameter(Up<'a, Parameter<'a>>),
	CantReassignLocal(Up<'a, Local<'a>>),

	//mv
	NotAnAbstractClass(Up<'a, ClassDeclaration<'a>>),
	ImplsMismatch { expected: &'a [AbstractMethod<'a>] },
	WrongImplParameters(Up<'a, AbstractMethod<'a>>),
}
impl<'a> NoDrop for Diag<'a> {}
impl<'d, 'a> Show for &'d Diag<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			Diag::CircularDependency { from, to } => {
				s.add("There is a circular dependency chain involving the link from ")?
					.add(from)?
					.add(" to ")?
					.add(to)?
					.add(".")?;
			}
			Diag::CantFindLocalModule { from, to } => {
				//TODO: mention what was tried?
				s.add("Can't resolve import from ")?
					.add(from)?
					.add(" to ")?
					.add(to)?;
			}
			Diag::ParseError(ref p) => {
				s.add(p)?;
			}
			Diag::CantCombineTypes(ref a, ref b) => {
				s.add("Unable to find a common type between ")?
					.add(a)?
					.add(" and ")?
					.add(b)?
					.add(".")?;
			}
			Diag::NotAssignable { ref expected, ref actual } => {
				s.add("Expecting a ")?
					.add(expected)?
					.add(", got a ")?
					.add(actual)?
					.add(".")?;
			}
			Diag::MemberNotFound(cls, name) => {
				s.add("Class ")?
					.add(cls.name)?
					.add(" does not have a member named ")?
					.add(name)?
					.add(".")?;
			}
			Diag::CantAccessSlotFromStaticMethod(slot) => {
				s.add("Currently in a static method; can't access ")?
					.add(slot.name)?
					.add(" as it is a slot stored on an instance.")?;
			}
			Diag::MissingEffectToGetSlot(slot) => {
				s.add("Can't read mutable slot ")?
					.add(slot.name)?
					.add(" through a pure reference; need '")?
					.add(Effect::Get)?
					.add("' effect.")?;
			}
			Diag::MissingEffectToSetSlot { allowed_effect, slot } => {
				s.add("Can't write to mutable slot ")?
					.add(slot.name)?
					.add(" through a ")?
					.add(allowed_effect)?
					.add(" reference; need '")?
					.add(Effect::Set)?
					.add("' effect.")?;
			}
			Diag::MethodUsedAsValue => {
				s.add("Attempt to use a method as a value; currently lambdas are not supported ")?
					.add("and methods may only be used as the left-hand side of a call expression.")?;
			}
			Diag::CallsNonMethod => {
				s.add(
					"Attempt to call an expression; currently lambdas are not supported and only methods may be called.",
				)?;
			}
			Diag::CantAccessStaticMethodThroughInstance(method) => {
				s.add(method.name())?
					.add(" is a static method; it should be prefixed with the name of the current class.")?;
			}
			Diag::IllegalSelfEffect { target_effect, method } => {
				s.add("Target has only a '")?
					.add(target_effect)?
					.add("' effect, but method ")?
					.add(method.name())?
					.add(" requires a '")?
					.add(method.self_effect())?
					.add("' effect.")?;
			}
			Diag::ArgumentCountMismatch(method, actual) => {
				s.add("Method ")?
					.add(method.name())?
					.add(" takes ")?
					.add(method.arity())?
					.add(" arguments; ")?
					.add(actual)?
					.add(" provided.")?;
			}
			Diag::ClassNotFound(name) => {
				s.add("Class ")?.add(name)?.add(" not found.")?;
			}
			Diag::StaticMethodNotFound(class, name) => {
				s.add("Class ")?
					.add(class.name)?
					.add(" has no static method ")?
					.add(name)?
					.add(".")?;
			}
			Diag::CantCallInstanceMethodFromStaticMethod(method) => {
				s.add("Method ")?
					.add(method.name())?
					.add(" is an instance method; can't call it from a static method.")?;
			}
			Diag::NotATailCall => {
				s.add("'recur' must occur in a tail call position.")?;
			}
			Diag::NewInvalid(class) => {
				s.add("Class ")?
					.add(class.name)?
					.add(" is not a 'slots' class; can't use 'new'.")?;
			}
			Diag::NewArgumentCountMismatch { class, n_slots, n_arguments } => {
				s.add("Class ")?
					.add(class.name)?
					.add(" has ")?
					.add(n_slots)?
					.add(" slots, but provided ")?
					.add(n_arguments)?
					.add(" arguments to 'new'.")?;
			}
			Diag::CantSetNonSlot(member) => {
				s.add(member.name())?
					.add(" is not a slot; can't write to it.")?;
			}
			Diag::SlotNotMutable(slot) => {
				s.add(slot.name)?.add(" is not mutable; can't write to it.")?;
			}
			Diag::CantReassignParameter(parameter) => {
				s.add("Can't reassign parameter ")?
					.add(parameter.name)?
					.add(".")?;
			}
			Diag::CantReassignLocal(local) => {
				//TODO:suggest using a Cell.
				s.add("Can't reassign local ")?.add(local.name)?.add(".")?;
			}
			Diag::NotAnAbstractClass(class) => {
				s.add("Class ")?.add(class.name)?.add(" is not abstract.")?;
			}
			Diag::ImplsMismatch { expected } => {
				s.add("Must implement abstract methods in their declaration order. Expected: ")?
					.join_map(expected, |a| a.name())?
					.add(".")?;
			}
			Diag::WrongImplParameters(abs) => {
				s.add("Parameter names must be exactly: ")?
					.join_map(abs.parameters(), |p| p.name)?;
			}
		}
		Ok(())
	}
}

pub fn show_diagnostics<'a, S: Shower>(module: &ModuleOrFail<'a>, s: &mut S) -> Result<(), S::Error> {
	let diags = module.diagnostics();
	assert!(!diags.is_empty()); // Else don't call this.

	let source = module.source();
	let text = source.text();
	let lc = LineAndColumnGetter::new(text);
	for &Diagnostic { loc, ref diag } in diags {
		let lc_loc = lc.line_and_column_at_loc(loc);
		s.add(source)?.add(&lc_loc)?.add(": ")?.add(diag)?;
	}
	Ok(())
}
