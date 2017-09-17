use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::loc::Loc;
use util::sym::Sym;
use util::u8_slice_ops::U8SliceOps;
use util::up::{SerializeUp, Up};

use super::class::SlotDeclaration;
use super::method::{InstMethod, MethodOrImpl, Parameter};
use super::ty::Ty;

pub enum LiteralValue<'a> {
	Nat(u32),
	Int(i32),
	Float(f64),
	String(&'a [u8]),
}
impl<'a> NoDrop for LiteralValue<'a> {}
impl<'a> Serialize for LiteralValue<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		match *self {
			LiteralValue::Nat(n) => serializer.serialize_u32(n),
			LiteralValue::Int(i) => serializer.serialize_i32(i),
			LiteralValue::Float(f) => serializer.serialize_f64(f),
			LiteralValue::String(s) => serializer.serialize_str(&s.clone_to_utf8_string()),
		}
	}
}


#[derive(Serialize)]
pub enum Pattern<'a> {
	Ignore,
	//TODO:PERF should own the local, not reference it
	Single(&'a Local<'a>),
	Destruct(Loc, &'a [Pattern<'a>]),
}
impl<'a> NoDrop for Pattern<'a> {}

#[derive(Serialize)]
pub struct Local<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for Local<'a> {}
impl<'a> SerializeUp for Local<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct Case<'a>(pub Loc, /*test*/ pub &'a Expr<'a>, /*result*/ pub &'a Expr<'a>);
impl<'a> NoDrop for Case<'a> {}

#[derive(Serialize)]
//TODO:PERF should own its local and expr, not reference them
pub struct Catch<'a>(pub Loc, /*caught*/ pub &'a Local<'a>, /*result*/ pub &'a Expr<'a>);
impl<'a> NoDrop for Catch<'a> {}

#[derive(Serialize)]
pub struct Expr<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub data: ExprData<'a>,
}
impl<'a> Expr<'a> {
	//pub fn children(&self) -> &[Self] {
	//	self.1.children()
	//}
}
impl<'a> NoDrop for Expr<'a> {}

#[derive(Serialize)]
pub enum ExprData<'a> {
	BogusCast(&'a Expr<'a>),
	Bogus,
	AccessParameter(Up<'a, Parameter<'a>>),
	AccessLocal(Up<'a, Local<'a>>),
	Let(Pattern<'a>, &'a Expr<'a>, &'a Expr<'a>),
	Seq(&'a Expr<'a>, &'a Expr<'a>),
	Literal(LiteralValue<'a>),
	IfElse { test: &'a Expr<'a>, then: &'a Expr<'a>, elze: &'a Expr<'a> },
	WhenTest(&'a [Case<'a>], &'a Expr<'a>),
	Try { body: &'a Expr<'a>, catch: Option<Catch<'a>>, finally: Option<&'a Expr<'a>> },
	For {
		local: Local<'a>,
		looper: &'a Expr<'a>,
		body: &'a Expr<'a>,
		provided_ty: Ty<'a>,
		received_ty: Ty<'a>,
	},
	//TODO:PERF should own InstMethod and args, not reference them
	StaticMethodCall { method: &'a InstMethod<'a>, args: &'a [&'a Expr<'a>] },
	InstanceMethodCall { target: &'a Expr<'a>, method: &'a InstMethod<'a>, args: &'a [&'a Expr<'a>] },
	MyInstanceMethodCall { method: &'a InstMethod<'a>, args: &'a [&'a Expr<'a>] },
	// We store the Ty here instead of an InstClass so we can easily get a reference to it;
	// It should always by Ty::Plain(EFFECT_MAX, some_InstClass).
	New(&'a [&'a Expr<'a>]),
	ArrayLiteral(&'a [&'a Expr<'a>]),
	GetMySlot(Up<'a, SlotDeclaration<'a>>),
	GetSlot(&'a Expr<'a>, Up<'a, SlotDeclaration<'a>>),
	SetSlot(Up<'a, SlotDeclaration<'a>>, &'a Expr<'a>),
	SelfExpr,
	Assert(&'a Expr<'a>),
	Recur(MethodOrImpl<'a>, &'a [&'a Expr<'a>]),
}
impl<'a> NoDrop for ExprData<'a> {}
impl<'a> ExprData<'a> {
	/*pub fn children(&self) -> &'a [&'a Expr<'a>] {
		match *self {
			ExprData::Let(_, ref a, ref b)
				| ExprData::Seq(ref a, ref b)
				=> Arr::_2(a, b),
			ExprData::IfElse { ref test, ref then, ref elze, .. } =>
				Arr::_3(test, then, elze),
			ExprData::WhenTest(ref cases, ref elze, _) => {
				unimplemented!()
				/*let mut b = ArrBuilder::<&Expr>::new();
				for &Case(_, ref test, ref result) in cases.iter() {
					b.add(test);
					b.add(result)
				}
				b.add(elze);
				b.finish()
				*/
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
			ExprData::For { ref looper, ref body, .. } => Arr::_2(looper, body),
			ExprData::InstanceMethodCall(ref target, _, ref args, _) => {
				//let mut b = ArrBuilder::<&Expr>::new();
				//b.add(target);
				//add_refs(&mut b, args);
				//b.finish()
				unimplemented!()
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
				=> unimplemented!(),//make_refs(args),
			ExprData::Bogus
				| ExprData::AccessParameter(_, _)
				| ExprData::AccessLocal(_)
				| ExprData::Literal(_)
				| ExprData::GetMySlot(_, _)
				| ExprData::SelfExpr(_)
				=> Arr::empty(),
		}
	}*/
}
