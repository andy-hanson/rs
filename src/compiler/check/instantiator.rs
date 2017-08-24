use util::arr::Arr;
use util::ptr::{Own, Ptr};

use super::super::model::method::InstMethod;
use super::super::model::ty::{InstCls, Ty, TypeParameter};

struct Inner(Ptr<TypeParameter>, Ty);
impl Clone for Inner {
	fn clone(&self) -> Inner {
		Inner(self.0.clone_ptr(), self.1.clone())
	}
}

pub struct Instantiator(Arr<Inner>);
impl Instantiator {
	pub fn nil() -> Instantiator {
		Instantiator(Arr::empty())
	}

	pub fn of_inst_cls(&InstCls(ref class, ref type_arguments): &InstCls) -> Instantiator {
		new(&class.type_parameters, type_arguments)
	}

	pub fn of_inst_method(&InstMethod(ref decl, ref type_arguments): &InstMethod) -> Instantiator {
		new(decl.type_parameters(), type_arguments)
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

	pub fn combine(&self, other: &Instantiator) -> Instantiator {
		Instantiator(self.0.concat(&other.0))
	}

	/*pub fn add_inst_cls(&mut self, (class_declaration, type_arguments): InstCls) {
		self.add(class_declaration.type_parameters, type_arguments)
	}

	fn add(&mut self, new_type_parameters: Arr<Ref<TypeParameter>>, new_type_arguments: Arr<Ty>) {
		assert!(new_type_parameters.len() == new_type_arguments.len());
		for n in new_type_parameters {
			self.type_parameters.push(n)
		}
		for n in new_type_arguments {
			self.type_arguments.push(n)
		}
	}*/
}

fn new(type_parameters: &Arr<Own<TypeParameter>>, type_arguments: &Arr<Ty>) -> Instantiator {
	assert_eq!(type_parameters.len(), type_arguments.len());
	let s = type_parameters.zip(type_arguments, |tp, ta| Inner(tp.ptr(), ta.clone()));
	Instantiator(s)
}
