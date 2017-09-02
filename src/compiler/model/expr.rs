use super::class::SlotDeclaration;
use super::method::{InstMethod, MethodOrImpl, Parameter};
use super::ty::Ty;

use util::arr::{Arr, ArrBuilder};
use util::loc::Loc;
use util::ptr::{Own, Ptr};
use util::sym::Sym;

pub enum LiteralValue {
	Pass,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	String(Arr<u8>),
}
impl LiteralValue {
	fn ty(&self) -> &Ty {
		match *self {
			LiteralValue::Pass => unimplemented!(),
			LiteralValue::Bool(_) => unimplemented!(),
			LiteralValue::Nat(_) => unimplemented!(),
			LiteralValue::Int(_) => unimplemented!(),
			LiteralValue::Float(_) => unimplemented!(),
			LiteralValue::String(_) => unimplemented!(),
		}
	}
}

pub enum Pattern {
	Ignore,
	Single(Own<Local>),
	Destruct(Loc, Arr<Pattern>),
}

pub struct Local {
	pub loc: Loc,
	pub ty: Ty,
	pub name: Sym,
}

pub struct Case(pub Loc, /*test*/ pub Box<Expr>, /*result*/ pub Box<Expr>);

pub struct Catch(pub Loc, /*caught*/ pub Own<Local>, /*result*/ pub Box<Expr>);

pub struct For {
	local: Own<Local>,
	looper: Box<Expr>,
	body: Box<Expr>,
	provided_ty: Ty,
	received_ty: Ty,
	result_ty: Ty,
}

pub struct Expr(pub Loc, pub ExprData);
impl Expr {
	pub fn loc(&self) -> Loc {
		self.0
	}

	pub fn ty(&self) -> &Ty {
		self.1.ty()
	}

	pub fn children(&self) -> Arr<&Expr> {
		self.1.children()
	}
}

pub enum ExprData {
	BogusCast(Ty, Box<Expr>),
	Bogus,
	AccessParameter(Ptr<Parameter>, Ty),
	AccessLocal(Ptr<Local>),
	Let(Pattern, Box<Expr>, Box<Expr>),
	Seq(Box<Expr>, Box<Expr>),
	Literal(LiteralValue),
	IfElse { test: Box<Expr>, then: Box<Expr>, elze: Box<Expr>, ty: Ty },
	WhenTest(Arr<Case>, Box<Expr>, Ty),
	Try { body: Box<Expr>, catch: Option<Catch>, finally: Option<Box<Expr>>, ty: Ty },
	For(For),
	StaticMethodCall(InstMethod, Arr<Expr>, Ty),
	InstanceMethodCall(Box<Expr>, InstMethod, Arr<Expr>, Ty),
	MyInstanceMethodCall(InstMethod, Arr<Expr>, Ty),
	// We store the Ty here instead of an InstCls so we can easily get a reference to it;
	// It should always by Ty::Plain(EFFECT_MAX, some_InstCls).
	New(Ty, Arr<Expr>),
	ArrayLiteral { element_ty: Ty, elements: Arr<Expr> },
	GetMySlot(Ptr<SlotDeclaration>, Ty),
	GetSlot(Box<Expr>, Ptr<SlotDeclaration>, Ty),
	SetSlot(Ptr<SlotDeclaration>, Box<Expr>),
	SelfExpr(Ty),
	Assert(Box<Expr>),
	//RecurMethod(Ptr<MethodWithBody>, Arr<Expr>),
	//RecurImpl(Ptr<Impl>, Arr<Expr>),
	Recur(MethodOrImpl, Arr<Expr>),
}
impl ExprData {
	pub fn ty(&self) -> &Ty {
		match *self {
			ExprData::BogusCast(ref ty, _)
			| ExprData::AccessParameter(_, ref ty)
			| ExprData::IfElse { ref ty, .. }
			| ExprData::WhenTest(_, _, ref ty)
			| ExprData::Try { ref ty, .. }
			| ExprData::StaticMethodCall(_, _, ref ty)
			| ExprData::InstanceMethodCall(_, _, _, ref ty)
			| ExprData::MyInstanceMethodCall(_, _, ref ty)
			| ExprData::New(ref ty, _)
			| ExprData::GetMySlot(_, ref ty)
			| ExprData::GetSlot(_, _, ref ty)
			| ExprData::SelfExpr(ref ty) => ty,
			ExprData::Bogus => Ty::bogus_ref(),
			ExprData::AccessLocal(ref local) => &local.ty,
			ExprData::Let(_, _, ref then) | ExprData::Seq(_, ref then) => then.ty(),
			ExprData::Literal(ref v) => v.ty(),
			ExprData::For(ref f) => &f.result_ty,
			ExprData::ArrayLiteral { .. } => unimplemented!(), //array[array type]
			ExprData::SetSlot(_, _) => unimplemented!(),       //void
			ExprData::Assert(_) => unimplemented!(),           //void
			//&ExprData::RecurMethod(ref method, _) => method.return_ty(),
			//&ExprData::RecurImpl(ref imp, _) => imp.implemented.return_ty(),
			ExprData::Recur(ref m, _) => m.return_ty(),
		}
	}

	pub fn children(&self) -> Arr<&Expr> {
		match *self {
			ExprData::Let(_, ref a, ref b)
				| ExprData::Seq(ref a, ref b)
				=> Arr::_2(a, b),
			ExprData::IfElse { ref test, ref then, ref elze, .. } =>
				Arr::_3(test, then, elze),
			ExprData::WhenTest(ref cases, ref elze, _) => {
				let mut b = ArrBuilder::<&Expr>::new();
				for &Case(_, ref test, ref result) in cases.iter() {
					b.add(test);
					b.add(result)
				}
				b.add(elze);
				b.finish()
			}
			ExprData::Try { ref body, ref catch, ref finally, .. } =>
				match *catch {
					Some(Catch(_, _, ref catch_result)) =>
						match *finally {
							Some(ref f) => Arr::_3(body, catch_result, f),
							None => Arr::_2(body, catch_result)
						},
					None =>
						match *finally { Some(ref f) => Arr::_2(body, f), None => Arr::_1(body) }
				},
			ExprData::For(ref f) => Arr::_2(&f.looper, &f.body),
			ExprData::InstanceMethodCall(ref target, _, ref args, _) => {
				let mut b = ArrBuilder::<&Expr>::new();
				b.add(target);
				add_refs(&mut b, args);
				b.finish()
			}
			ExprData::GetSlot(ref e, _, _)
				| ExprData::SetSlot(_, ref e)
				| ExprData::Assert(ref e)
				| ExprData::BogusCast(_, ref e)
				=> Arr::_1(e),
			ExprData::ArrayLiteral { elements: ref args, .. }
				| ExprData::New(_, ref args)
				| ExprData::MyInstanceMethodCall(_, ref args, _)
				| ExprData::StaticMethodCall(_, ref args, _)
				//| &ExprData::RecurMethod(_, ref args)
				//| &ExprData::RecurImpl(_, ref args)
				| ExprData::Recur(_, ref args)
				=> make_refs(args),
			ExprData::Bogus
				| ExprData::AccessParameter(_, _)
				| ExprData::AccessLocal(_)
				| ExprData::Literal(_)
				| ExprData::GetMySlot(_, _)
				| ExprData::SelfExpr(_)
				=> Arr::empty(),
		}
	}
}

fn make_refs<T>(arr: &Arr<T>) -> Arr<&T> {
	let mut b = ArrBuilder::<&T>::new();
	add_refs(&mut b, arr);
	b.finish()
}

fn add_refs<'a, T>(b: &mut ArrBuilder<&'a T>, arr: &'a Arr<T>) {
	for a in arr.iter() {
		b.add(a)
	}
}
