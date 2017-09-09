use util::arena::{List, NoDrop};
use util::loc::Loc;
use util::path::{Path, RelPath};
use util::sym::Sym;

use super::super::super::model::effect::Effect;

pub struct Module<'a> {
	pub imports: List<'a, Import<'a>>,
	pub class: &'a Class<'a>,
}
impl<'a> NoDrop for Module<'a> {}

pub enum Import<'a> {
	Global(Loc, Path<'a>),
	Local(Loc, RelPath<'a>),
}
impl<'a> NoDrop for Import<'a> {}

pub struct Class<'a> {
	pub loc: Loc,
	pub type_parameters: &'a [Sym],
	pub head: Option<ClassHead<'a>>,
	pub supers: List<'a, Super<'a>>,
	pub methods: List<'a, Method<'a>>,
}
impl<'a> NoDrop for Class<'a> {}

pub struct ClassHead<'a>(pub Loc, pub ClassHeadData<'a>);
impl<'a> NoDrop for ClassHead<'a> {}
pub enum ClassHeadData<'a> {
	Abstract(List<'a, AbstractMethod<'a>>),
	Slots(List<'a, Slot<'a>>),
	Builtin,
}
impl<'a> NoDrop for ClassHeadData<'a> {}

pub struct AbstractMethod<'a> {
	pub loc: Loc,
	pub type_parameters: &'a [Sym],
	pub return_ty: Ty<'a>,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: List<'a, Parameter<'a>>,
}
impl<'a> NoDrop for AbstractMethod<'a> {}

pub struct Slot<'a> {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for Slot<'a> {}

pub struct Super<'a> {
	pub loc: Loc,
	pub name: Sym,
	pub ty_args: List<'a, Ty<'a>>,
	pub impls: List<'a, Impl<'a>>,
}
impl<'a> NoDrop for Super<'a> {}

pub struct Impl<'a> {
	pub loc: Loc,
	pub name: Sym,
	pub parameter_names: &'a [Sym],
	// May be missing for a builtin
	pub body: Option<&'a Expr<'a>>, //TODO:PERF doesn't actually need to be a reference...
}
impl<'a> NoDrop for Impl<'a> {}

pub struct Method<'a> {
	pub loc: Loc,
	pub is_static: bool,
	pub type_parameters: &'a [Sym],
	pub return_ty: Ty<'a>,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: List<'a, Parameter<'a>>,
	pub body: Option<&'a Expr<'a>>, //TODO:PERF doesn't actually need to be a reference...
}
impl<'a> NoDrop for Method<'a> {}

pub struct Parameter<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for Parameter<'a> {}

pub struct Ty<'a> {
	pub loc: Loc,
	pub effect: Effect,
	pub name: Sym,
	pub ty_args: List<'a, Ty<'a>>,
}
impl<'a> NoDrop for Ty<'a> {}

pub struct Pattern<'a>(pub Loc, pub PatternData<'a>);
impl<'a> NoDrop for Pattern<'a> {}
pub enum PatternData<'a> {
	Ignore,
	Single(Sym),
	Destruct(&'a Pattern<'a>),
}
impl<'a> NoDrop for PatternData<'a> {}

pub struct Expr<'a>(pub Loc, pub ExprData<'a>);
impl<'a> NoDrop for Expr<'a> {}

pub enum ExprData<'a> {
	Access(Sym),
	StaticAccess(/*class_name*/ Sym, /*static_method_name*/ Sym),
	OperatorCall(&'a Expr<'a>, Sym, &'a Expr<'a>),
	TypeArguments(&'a Expr<'a>, List<'a, Ty<'a>>),
	//TODO:PERF These should use List<Expr>, not List<&Expr>
	Call(&'a Expr<'a>, List<'a, &'a Expr<'a>>),
	Recur(List<'a, &'a Expr<'a>>),
	New(List<'a, Ty<'a>>, List<'a, &'a Expr<'a>>),
	ArrayLiteral(Option<Ty<'a>>, List<'a, &'a Expr<'a>>),
	GetProperty(&'a Expr<'a>, Sym),
	SetProperty(Sym, &'a Expr<'a>),
	// This one shouldn't escape parse_expr.
	LetInProgress(Pattern<'a>, &'a Expr<'a>),
	//TODO:PERF pattern should be owned, not a reference
	Let(&'a Pattern<'a>, &'a Expr<'a>, &'a Expr<'a>),
	Seq(&'a Expr<'a>, &'a Expr<'a>),
	Literal(LiteralValue<'a>),
	SelfExpr,
	IfElse(&'a Expr<'a>, &'a Expr<'a>, &'a Expr<'a>),
	WhenTest(List<'a, Case<'a>>, &'a Expr<'a>),
	Assert(&'a Expr<'a>),
	Try(&'a Expr<'a>, Option<Catch<'a>>, Option<&'a Expr<'a>>),
	For(Sym, /*looper*/ &'a Expr<'a>, /*body*/ &'a Expr<'a>),
}
impl<'a> NoDrop for ExprData<'a> {}

pub enum LiteralValue<'a> {
	Pass,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	String(&'a [u8]),
}
impl<'a> NoDrop for LiteralValue<'a> {}

//TODO:PERF Should own its Exprs, not reference them
pub struct Case<'a>(pub Loc, /*test*/ pub &'a Expr<'a>, /*result*/ pub &'a Expr<'a>);
impl<'a> NoDrop for Case<'a> {}

pub struct Catch<'a> {
	pub loc: Loc,
	pub exception_type: Ty<'a>,
	pub exception_name_loc: Loc,
	pub exception_name: Sym,
	pub then: &'a Expr<'a>,
}
impl<'a> NoDrop for Catch<'a> {}
