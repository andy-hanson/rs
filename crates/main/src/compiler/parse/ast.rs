use util::arena::{List, NoDrop};
use util::loc::Loc;
use util::sym::Sym;

use super::super::super::model::effect::Effect;

//#[derive(NoDrop)]
pub struct Module<'a> {
	pub imports: List<'a, Import<'a>>,
	pub class: &'a Class<'a>,
}
//TODO:use derive
//impl<'a, 't : 'a> NoDrop for Module<'a, 't> {
//	const NO_DROP_MARKER: u8 = 0;//Import::<'a, 't>::NO_DROP_MARKER + List::<'a, Import<'a,'t>>::NO_DROP_MARKER;
//}

#[derive(NoDrop)]
pub enum Import<'a> {
	Global(Loc, List<'a, &'a [u8]>),
	Local(Loc, /*n_parents*/ usize, List<'a, &'a [u8]>),
}
//TODO:use derive
//impl<'a, 't : 'a> NoDrop for Import<'a, 't> {
//	const NO_DROP_MARKER: u8 = 0;
//}

#[derive(NoDrop)]
pub struct Class<'a> {
	pub loc: Loc,
	pub type_parameters: &'a [Sym],
	pub head: Option<ClassHead<'a>>,
	pub supers: List<'a, Super<'a>>,
	pub methods: List<'a, Method<'a>>,
}

#[derive(NoDrop)]
pub struct ClassHead<'a>(pub Loc, pub ClassHeadData<'a>);
#[derive(NoDrop)]
pub enum ClassHeadData<'a> {
	Abstract(List<'a, AbstractMethod<'a>>),
	Slots(List<'a, Slot<'a>>),
	Builtin,
}

#[derive(NoDrop)]
pub struct AbstractMethod<'a> {
	pub loc: Loc,
	pub type_parameters: &'a [Sym],
	pub return_ty: Ty<'a>,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: List<'a, Parameter<'a>>,
}

#[derive(NoDrop)]
pub struct Slot<'a> {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty<'a>,
	pub name: Sym,
}

#[derive(NoDrop)]
pub struct Super<'a> {
	pub loc: Loc,
	pub name: Sym,
	pub ty_args: List<'a, Ty<'a>>,
	pub impls: List<'a, Impl<'a>>,
}

#[derive(NoDrop)]
pub struct Impl<'a> {
	pub loc: Loc,
	pub name: Sym,
	pub parameter_names: &'a [Sym],
	// May be missing for a builtin
	pub body: Option<&'a Expr<'a>>, //TODO:PERF doesn't actually need to be a reference...
}

#[derive(NoDrop)]
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

#[derive(NoDrop)]
pub struct Parameter<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub name: Sym,
}

#[derive(NoDrop)]
pub struct Ty<'a> {
	pub loc: Loc,
	pub effect: Effect,
	pub name: Sym,
	pub ty_args: List<'a, Ty<'a>>,
}

#[derive(NoDrop)]
pub struct Pattern<'a>(pub Loc, pub PatternData<'a>);
#[derive(NoDrop)]
pub enum PatternData<'a> {
	Ignore,
	Single(Sym),
	Destruct(&'a Pattern<'a>),
}

#[derive(NoDrop)]
pub struct Expr<'a>(pub Loc, pub ExprData<'a>);

#[derive(NoDrop)]
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

#[derive(NoDrop)]
pub enum LiteralValue<'a> {
	Pass,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	String(&'a [u8]),
}

#[derive(NoDrop)]
//TODO:PERF Should own its Exprs, not reference them
pub struct Case<'a>(pub Loc, /*test*/ pub &'a Expr<'a>, /*result*/ pub &'a Expr<'a>);

#[derive(NoDrop)]
pub struct Catch<'a> {
	pub loc: Loc,
	pub exception_type: Ty<'a>,
	pub exception_name_loc: Loc,
	pub exception_name: Sym,
	pub then: &'a Expr<'a>,
}
