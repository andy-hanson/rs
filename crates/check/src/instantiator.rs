use util::late::Late;
use util::up::Up;

use model::class::InstClass;
use model::method::InstMethod;
use model::ty::{Ty, TypeParameter};

pub trait InstantiatorLike<'model> {
	fn replace(&self, tp: Up<'model, TypeParameter<'model>>) -> Option<Ty<'model>>;

	fn replace_or_same(&self, ty: Up<'model, TypeParameter<'model>>) -> Ty<'model> {
		self.replace(ty).unwrap_or_else(|| Ty::Param(ty))
	}
}

#[derive(Copy, Clone)]
pub struct Instantiator<'model> {
	params: &'model [TypeParameter<'model>],
	args: &'model [Late<Ty<'model>>],
}
impl<'model> Instantiator<'model> {
	pub const NIL: Self = Instantiator { params: &[], args: &[] };

	fn new(params: &'model [TypeParameter<'model>], args: &'model [Late<Ty<'model>>]) -> Self {
		Instantiator { params, args }
	}

	pub fn of_inst_class(&InstClass { class, ty_args }: &InstClass<'model>) -> Self {
		Self::new(class.type_parameters, ty_args)
	}

	pub fn of_inst_method(m: &InstMethod<'model>) -> Self {
		Self::new(m.method_decl.type_parameters(), m.ty_args)
	}
}
impl<'model> InstantiatorLike<'model> for Instantiator<'model> {
	fn replace(&self, tp: Up<'model, TypeParameter<'model>>) -> Option<Ty<'model>> {
		self.params
			.iter()
			.position(|p| tp.ptr_eq(Up(p)))
			.map(|i| self.args[i].clone())
	}
}
