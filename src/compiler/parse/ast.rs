use compiler::model::effect::Effect;
use compiler::model::LiteralValue;
use util::arr::Arr;
use util::loc::Loc;
use util::path::{ Path, RelPath };
use util::sym::Sym;

pub struct Module {
	pub imports: Arr<Import>,
	pub class: Box<ClassDeclaration>,
}
impl Module {
	pub fn of(imports: Arr<Import>, class: ClassDeclaration) -> Module {
		Module { imports, class: bx(class) }
	}
}

pub enum Import {
	Global(Loc, Path),
	Local(Loc, RelPath),
}

pub struct ClassDeclaration {
	pub loc: Loc,
	pub type_parameters: Arr<Sym>,
	pub head: Option<Head>,
	pub supers: Arr<Super>,
	pub methods: Arr<Method>,
}
impl ClassDeclaration {
	pub fn of(loc: Loc, type_parameters: Arr<Sym>, head: Option<Head>, supers: Arr<Super>, methods: Arr<Method>) -> ClassDeclaration {
		ClassDeclaration { loc, type_parameters, head, supers, methods }
	}
}

pub struct Head(Loc, HeadData);
impl Head {
	pub fn abstract_head(loc: Loc, methods: Arr<AbstractMethod>) -> Head {
		Head(loc, HeadData::Abstract(methods))
	}

	pub fn slots(loc: Loc, slots: Arr<Slot>) -> Head {
		Head(loc, HeadData::Slots(slots))
	}
}
pub enum HeadData {
	Abstract(Arr<AbstractMethod>),
	Slots(Arr<Slot>),
}

pub struct AbstractMethod {
	pub loc: Loc,
	pub type_parameters: Arr<Sym>,
	pub return_ty: Ty,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: Arr<Parameter>,
}
impl AbstractMethod {
	pub fn of(loc: Loc, type_parameters: Arr<Sym>, return_ty: Ty, name: Sym, self_effect: Effect, parameters: Arr<Parameter>) -> AbstractMethod {
		AbstractMethod { loc, type_parameters, return_ty, name, self_effect, parameters }
	}
}

pub struct Slot {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty,
	pub name: Sym,
}
impl Slot {
	pub fn of(loc: Loc, mutable: bool, ty: Ty, name: Sym) -> Slot {
		Slot { loc, mutable, ty, name }
	}
}

pub struct Super {
	pub loc: Loc,
	pub name: Sym,
	pub ty_args: Arr<Ty>,
	pub impls: Arr<Impl>,
}
impl Super {
	pub fn of(loc: Loc, name: Sym, ty_args: Arr<Ty>, impls: Arr<Impl>) -> Super {
		Super { loc, name, ty_args, impls }
	}
}

pub struct Impl {
	pub loc: Loc,
	pub name: Sym,
	pub parameter_names: Arr<Sym>,
	pub body: Expr,
}
impl Impl {
	pub fn of(loc: Loc, name: Sym, parameter_names: Arr<Sym>, body: Expr) -> Impl {
		Impl { loc, name, parameter_names, body }
	}
}

pub struct Method {
	pub loc: Loc,
	pub is_static: bool,
	pub type_parameters: Arr<Sym>,
	pub return_ty: Ty,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: Arr<Parameter>,
	pub body: Expr,
}
impl Method {
	pub fn of(
		loc: Loc, is_static: bool, type_parameters: Arr<Sym>, return_ty: Ty,
		name: Sym, self_effect: Effect, parameters: Arr<Parameter>, body: Expr) -> Method {
		Method { loc, is_static, type_parameters, return_ty, name, self_effect, parameters, body }
	}
}

pub struct Parameter { pub loc: Loc, pub ty: Ty, pub name: Sym }

pub struct Ty {
	pub loc: Loc,
	pub effect: Effect,
	pub name: Sym,
	pub ty_args: Arr<Ty>,
}
impl Ty {
	pub fn of(loc: Loc, effect: Effect, name: Sym, ty_args: Arr<Ty>) -> Ty {
		Ty { loc, effect, name, ty_args }
	}
}

pub struct Pattern {
	pub loc: Loc,
	pub data: PatternData,
}
impl Pattern {
	pub fn single(loc: Loc, sym: Sym) -> Pattern {
		Pattern { loc, data: PatternData::Single(sym) }
	}
}
pub enum PatternData {
	Ignore,
	Single(Sym),
	Destruct(Arr<Pattern>),
}

fn expr(loc: Loc, data: ExprData) -> Expr { Expr { loc, data } }
fn bx<T>(t: T) -> Box<T> { Box::new(t) }

pub struct Expr {
	pub loc: Loc,
	pub data: ExprData
}
impl Expr {
	pub fn access(loc: Loc, sym: Sym) -> Expr {
		expr(loc, ExprData::Access(sym))
	}

	pub fn static_access(loc: Loc, class_name: Sym, static_method_name: Sym) -> Expr {
		expr(loc, ExprData::StaticAccess(class_name, static_method_name))
	}

	pub fn operator_call(loc: Loc, left: Expr, operator: Sym, right: Expr) -> Expr {
		expr(loc, ExprData::OperatorCall(bx(left), operator, bx(right)))
	}

	pub fn type_arguments(loc: Loc, target: Expr, args: Arr<Ty>) -> Expr {
		expr(loc, ExprData::TypeArguments(bx(target), args))
	}

	pub fn call(loc: Loc, target: Expr, args: Arr<Expr>) -> Expr {
		expr(loc, ExprData::Call(bx(target), args))
	}

	pub fn recur(loc: Loc, args: Arr<Expr>) -> Expr {
		expr(loc, ExprData::Recur(args))
	}

	pub fn new(loc: Loc, ty_args: Arr<Ty>, args: Arr<Expr>) -> Expr {
		expr(loc, ExprData::New(ty_args, args))
	}

	pub fn array_literal(loc: Loc, ty: Option<Ty>, args: Arr<Expr>) -> Expr {
		expr(loc, ExprData::ArrayLiteral(ty, args))
	}

	pub fn get_property(loc: Loc, target: Expr, property_name: Sym) -> Expr {
		expr(loc, ExprData::GetProperty(bx(target), property_name))
	}

	pub fn set_property(loc: Loc, property_name: Sym, value: Expr) -> Expr {
		expr(loc, ExprData::SetProperty(property_name, bx(value)))
	}

	pub fn let_in_progress(loc: Loc, pattern: Pattern, value: Expr) -> Expr {
		expr(loc, ExprData::LetInProgress(pattern, bx(value)))
	}

	pub fn let_expr(loc: Loc, pattern: Pattern, value: Box<Expr>, then: Expr) -> Expr {
		expr(loc, ExprData::Let(pattern, value, bx(then)))
	}

	pub fn seq(loc: Loc, first: Expr, then: Expr) -> Expr {
		expr(loc, ExprData::Seq(bx(first), bx(then)))
	}

	pub fn literal(loc: Loc, value: LiteralValue) -> Expr {
		expr(loc, ExprData::Literal(value))
	}

	pub fn self_expr(loc: Loc) -> Expr {
		expr(loc, ExprData::SelfExpr)
	}

	pub fn if_else(loc: Loc, condition: Expr, then: Expr, elze: Expr) -> Expr {
		expr(loc, ExprData::IfElse(bx(condition), bx(then), bx(elze)))
	}

	pub fn when_test(loc: Loc, cases: Arr<Case>, elze: Expr) -> Expr {
		expr(loc, ExprData::WhenTest(cases, bx(elze)))
	}

	pub fn assert(loc: Loc, condition: Expr) -> Expr {
		expr(loc, ExprData::Assert(bx(condition)))
	}

	pub fn try(loc: Loc, body: Expr, catch: Option<Catch>, finally: Option<Expr>) -> Expr {
		expr(loc, ExprData::Try(bx(body), catch, finally.map(bx)))
	}

	pub fn for_expr(loc: Loc, local_name: Sym, looper: Expr, body: Expr) -> Expr {
		expr(loc, ExprData::For(local_name, bx(looper), bx(body)))
	}
}

pub enum ExprData {
	Access(Sym),
	StaticAccess(/*class_name*/ Sym, /*static_method_name*/Sym),
	OperatorCall(Box<Expr>, Sym, Box<Expr>),
	TypeArguments(Box<Expr>, Arr<Ty>),
	Call(Box<Expr>, Arr<Expr>),
	Recur(Arr<Expr>),
	New(Arr<Ty>, Arr<Expr>),
	ArrayLiteral(Option<Ty>, Arr<Expr>),
	GetProperty(Box<Expr>, Sym),
	SetProperty(Sym, Box<Expr>),
	// This one shouldn't escape parse_expr.
	LetInProgress(Pattern, Box<Expr>),
	Let(Pattern, Box<Expr>, Box<Expr>),
	Seq(Box<Expr>, Box<Expr>),
	Literal(LiteralValue),
	SelfExpr,
	IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
	WhenTest(Arr<Case>, Box<Expr>),
	Assert(Box<Expr>),
	Try(Box<Expr>, Option<Catch>, Option<Box<Expr>>),
	For(Sym, /*looper*/ Box<Expr>, /*body*/ Box<Expr>),
}

pub struct Case {
	pub loc: Loc,
	pub test: Expr,
	pub result: Expr,
}
impl Case {
	pub fn of(loc: Loc, test: Expr, result: Expr) -> Case {
		Case { loc, test, result }
	}
}

pub struct Catch {
	pub loc: Loc,
	pub exception_ty: Ty,
	pub exception_name_loc: Loc,
	pub exception_name: Sym,
	pub then: Box<Expr>,
}
impl Catch {
	pub fn of(loc: Loc, exception_ty: Ty, exception_name_loc: Loc, exception_name: Sym, then: Expr) -> Catch {
		Catch { loc, exception_ty, exception_name_loc, exception_name, then: bx(then) }
	}
}
