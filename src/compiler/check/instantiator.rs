use util::arr::{Arr, CloneSliceOps, SliceOps};
use util::ptr::{Own, Ptr};

use super::super::super::model::method::InstMethod;
use super::super::super::model::ty::{InstCls, Ty, TypeParameter};

struct Inner(Ptr<TypeParameter>, Ty);
impl Clone for Inner {
	fn clone(&self) -> Self {
		Inner(self.0.clone_ptr(), self.1.clone())
	}
}

pub struct Instantiator(Arr<Inner>);
impl Instantiator {
	pub fn nil() -> Self {
		Instantiator(Arr::empty())
	}

	pub fn of_inst_cls(&InstCls(ref class, ref type_arguments): &InstCls) -> Self {
		Self::new(&class.type_parameters, type_arguments)
	}

	pub fn of_inst_method(&InstMethod(ref decl, ref type_arguments): &InstMethod) -> Self {
		Self::new(decl.type_parameters(), type_arguments)
	}

	fn new(type_parameters: &[Own<TypeParameter>], type_arguments: &[Ty]) -> Self {
		Instantiator(type_parameters.zip(type_arguments, |tp, ta| Inner(tp.ptr(), ta.clone())))
	}

	pub fn replace_or_same(&self, ty: &Ptr<TypeParameter>) -> Ty {
		self.replace(ty)
			.unwrap_or_else(|| Ty::Param(ty.clone_ptr()))
	}

	pub fn replace(&self, tp: &Ptr<TypeParameter>) -> Option<Ty> {
		for i in 0..self.0.len() {
			let &Inner(ref ty_parameter, ref replace_ty) = &self.0[i];
			if tp.fast_equals(ty_parameter) {
				return Some(replace_ty.clone())
			}
		}
		None
	}

	pub fn combine(&self, other: &Instantiator) -> Self {
		Instantiator(self.0.concat(&other.0))
	}
}
