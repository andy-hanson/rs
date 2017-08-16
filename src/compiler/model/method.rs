use util::arr::Arr;
use util::loc::Loc;
use util::ptr::{ Own, Ptr, LateOwn };
use util::sym::Sym;

use super::class_declaration::ClassDeclaration;
use super::effect::Effect;
use super::expr::Expr;
use super::ty::{ Ty, TypeParameter };

pub struct MethodSignature {
	class: Ptr<ClassDeclaration>,
	pub loc: Loc,
	pub name: Sym,
	pub type_parameters: Arr<Own<TypeParameter>>,
	pub return_ty: Ty,
	pub self_effect: Effect, // Ignore for static methods
	pub parameters: Arr<Parameter>,
}

pub struct AbstractMethod(MethodSignature);
impl AbstractMethod {
	pub fn name(&self) -> Sym {
		self.0.name
	}
}

pub struct MethodWithBody {
	pub is_static: bool,
	signature: MethodSignature,
	body: LateOwn<Expr>,
}
impl MethodWithBody {
	pub fn new(
		class: &Own<ClassDeclaration>, loc: Loc, is_static: bool, name: Sym,
		type_parameters: Arr<Own<TypeParameter>>, return_ty: Ty, self_effect: Effect,
		parameters: Arr<Parameter>) -> MethodWithBody {
		MethodWithBody {
			is_static,
			signature: MethodSignature {
				class: class.ptr(),
				loc,
				name,
				type_parameters,
				return_ty,
				self_effect,
				parameters,
			},
			body: LateOwn::new(),
		}
	}

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

	pub fn parameters(&self) -> &Arr<Parameter> {
		&self.signature.parameters
	}

	pub fn body(&self) -> &Expr {
		&self.body
	}

	pub fn set_body(&mut self, expr: Expr) {
		self.body.init(expr)
	}
}

pub struct Parameter { pub loc: Loc, pub ty: Ty, pub name: Sym, pub index: u32 }
