use util::arr::Arr;
use util::loc::Loc;
use util::ptr::{ Own, Ptr, LateOwn };
use util::sym::Sym;

use super::class::ClassDeclaration;
use super::effect::Effect;
use super::expr::Expr;
use super::ty::{ Ty, TypeParameter };

pub struct MethodSignature {
	pub class: Ptr<ClassDeclaration>,
	pub loc: Loc,
	pub name: Sym,
	pub type_parameters: Arr<Own<TypeParameter>>,
	pub return_ty: Ty,
	pub self_effect: Effect, // Ignore for static methods
	pub parameters: Arr<Own<Parameter>>,
}

pub struct AbstractMethod(MethodSignature);
impl AbstractMethod {
	pub fn loc(&self) -> Loc {
		self.0.loc
	}

	pub fn name(&self) -> Sym {
		self.0.name
	}

	pub fn type_parameters(&self) -> &Arr<Own<TypeParameter>> {
		&self.0.type_parameters
	}

	pub fn return_ty(&self) -> &Ty {
		&self.0.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.0.self_effect
	}

	pub fn parameters(&self) -> &Arr<Own<Parameter>> {
		&self.0.parameters
	}
}

pub struct MethodWithBody {
	pub is_static: bool,
	pub signature: MethodSignature,
	// Optional because this might be a builtin method
	pub body: LateOwn<Option<Expr>>,
}
impl MethodWithBody {
	pub fn loc(&self) -> Loc {
		self.signature.loc
	}

	pub fn name(&self) -> Sym {
		self.signature.name
	}

	pub fn type_parameters(&self) -> &Arr<Own<TypeParameter>> {
		&self.signature.type_parameters
	}

	pub fn return_ty(&self) -> &Ty {
		&self.signature.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.signature.self_effect
	}

	pub fn parameters(&self) -> &Arr<Own<Parameter>> {
		&self.signature.parameters
	}

	pub fn body(&self) -> &Option<Expr> {
		&self.body
	}

	pub fn set_body(&self, expr: Option<Expr>) {
		self.body.init(expr)
	}
}

pub struct Parameter { pub loc: Loc, pub ty: Ty, pub name: Sym, pub index: u32 }

pub struct MethodInst(pub MethodOrAbstract, pub Arr<Ty>);

pub struct Impl {
	pub implemented: Ptr<AbstractMethod>,
	pub body: LateOwn<Expr>,
}

pub enum MethodOrImpl {
	Method(Ptr<MethodWithBody>),
	Impl(Ptr<Impl>),
}
impl MethodOrImpl {
	pub fn copy(&self) -> MethodOrImpl {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrImpl::Method(m.clone_ptr()),
			MethodOrImpl::Impl(ref i) => MethodOrImpl::Impl(i.clone_ptr()),
		}
	}

	pub fn method_or_abstract(&self) -> MethodOrAbstract {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrAbstract::Method(m.clone_ptr()),
			MethodOrImpl::Impl(ref i) => MethodOrAbstract::Abstract(i.implemented.clone_ptr()),
		}
	}

	pub fn return_ty(&self) -> &Ty {
		&self.signature().return_ty
	}

	pub fn signature(&self) -> &MethodSignature {
		match *self {
			MethodOrImpl::Method(ref method) =>
				&method.signature,
			MethodOrImpl::Impl(ref imp) =>
				&imp.implemented.0,
		}
	}
}

pub enum MethodOrAbstract {
	Method(Ptr<MethodWithBody>),
	Abstract(Ptr<AbstractMethod>),
}
impl MethodOrAbstract {
	pub fn copy(&self) -> MethodOrAbstract {
		match *self {
			MethodOrAbstract::Method(ref m) =>
				MethodOrAbstract::Method(m.clone_ptr()),
			MethodOrAbstract::Abstract(ref a) =>
				MethodOrAbstract::Abstract(a.clone_ptr()),
		}
	}

	pub fn is_static(&self) -> bool {
		match *self {
			MethodOrAbstract::Method(ref m) => m.is_static,
			MethodOrAbstract::Abstract(_) => false,
		}
	}

	pub fn type_parameters(&self) -> &Arr<Own<TypeParameter>> {
		match *self {
			MethodOrAbstract::Method(ref m) => m.type_parameters(),
			MethodOrAbstract::Abstract(ref a) => a.type_parameters(),
		}
	}

	pub fn return_ty(&self) -> &Ty {
		match *self {
			MethodOrAbstract::Method(ref m) => m.return_ty(),
			MethodOrAbstract::Abstract(ref a) => a.return_ty(),
		}
	}

	pub fn self_effect(&self) -> Effect {
		match *self {
			MethodOrAbstract::Method(ref m) => m.self_effect(),
			MethodOrAbstract::Abstract(ref a) => a.self_effect(),
		}
	}

	pub fn parameters(&self) -> &Arr<Own<Parameter>> {
		match *self {
			MethodOrAbstract::Method(ref m) => m.parameters(),
			MethodOrAbstract::Abstract(ref a) => a.parameters(),
		}
	}
}
