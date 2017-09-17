use util::up::Up;

use model::method::InstMethod;
use model::ty::{InstClass, Ty, TypeParameter};

pub struct Instantiator<'model>(&'model [TypeParameter<'model>], &'model [Ty<'model>]);
impl<'model> Instantiator<'model> {
	pub const NIL: Self = Instantiator(&[], &[]);

	fn new(params: &'model [TypeParameter<'model>], args: &'model [Ty<'model>]) -> Self {
		assert_eq!(params.len(), args.len());
		Instantiator(params, args)
	}

	pub fn of_inst_class(&InstClass { class, ty_args }: &InstClass<'model>) -> Self {
		Self::new(class.type_parameters, ty_args)
	}

	pub fn of_inst_method(&InstMethod { ref method_decl, ty_args }: &InstMethod<'model>) -> Self {
		Self::new(method_decl.type_parameters(), ty_args)
	}

	//fn new(type_parameters: &[Own<TypeParameter>], type_arguments: &[Ty]) -> Self {
	//	Instantiator(type_parameters.zip(type_arguments.into_iter(), |tp, ta| Inner(tp.ptr(), ta.clone())))
	//}

	pub fn replace_or_same(&self, ty: Up<'model, TypeParameter<'model>>) -> Ty<'model> {
		self.replace(ty).unwrap_or_else(|| Ty::Param(ty))
	}

	pub fn replace(&self, tp: Up<'model, TypeParameter<'model>>) -> Option<Ty<'model>> {
		for i in 0..self.0.len() {
			let ty_parameter = Up(&self.0[i]);
			if tp.ptr_eq(ty_parameter) {
				return Some(self.1[i].clone())
			}
		}
		None
	}

	pub fn combine(&self, other: &Self) -> Self {
		if self.is_empty() {
			other.cloned()
		} else if other.is_empty() {
			self.cloned()
		} else {
			//concat
			unimplemented!()
		}
	}

	fn cloned(&self) -> Self {
		Instantiator(self.0, self.1)
	}

	fn is_empty(&self) -> bool {
		self.0.is_empty()
	}
}
