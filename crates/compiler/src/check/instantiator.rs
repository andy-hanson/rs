use model::method::InstMethod;
use model::ty::{InstCls, Ty, TypeParameter};

pub struct Instantiator<'model>(&'model [TypeParameter<'model>], &'model [Ty<'model>]);
impl<'model> Instantiator<'model> {
	pub const NIL: Self = Instantiator(&[], &[]);

	fn new(params: &'model [TypeParameter<'model>], args: &'model [Ty<'model>]) -> Self {
		assert_eq!(params.len(), args.len());
		Instantiator(params, args)
	}

	pub fn of_inst_cls(&InstCls(class, type_arguments): &InstCls<'model>) -> Self {
		Self::new(class.type_parameters, type_arguments)
	}

	pub fn of_inst_method(&InstMethod(ref decl, type_arguments): &InstMethod<'model>) -> Self {
		Self::new(decl.type_parameters(), type_arguments)
	}

	//fn new(type_parameters: &[Own<TypeParameter>], type_arguments: &[Ty]) -> Self {
	//	Instantiator(type_parameters.zip(type_arguments.into_iter(), |tp, ta| Inner(tp.ptr(), ta.clone())))
	//}

	pub fn replace_or_same(&self, ty: &'model TypeParameter<'model>) -> Ty<'model> {
		self.replace(ty).unwrap_or_else(|| Ty::Param(ty))
	}

	pub fn replace(&self, tp: &'model TypeParameter<'model>) -> Option<Ty<'model>> {
		for i in 0..self.0.len() {
			//let &Inner(ref ty_parameter, ref replace_ty) = &self.0[i];
			let ty_parameter = &self.0[i];
			if tp.fast_equals(ty_parameter) {
				return Some(self.1[i].clone())
			}
		}
		None
	}

	pub fn combine(&self, other: &Self) -> Self {
		unused!(other);
		unimplemented!() //Instantiator(self.0.concat(&other.0))
	}
}
