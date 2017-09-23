use util::arena::Arena;
use util::iter::KnownLen;
use util::late::Late;
use util::up::Up;

use model::class::InstClass;
use model::method::{MethodOrImplOrAbstract, MethodWithBody};
use model::ty::{Ty, TypeParameter};

use super::instantiator::InstantiatorLike;

pub enum InferResult<'res, 'model: 'res> {
	AlreadyHave(&'res Ty<'model>),
	JustFilledIn,
	NotInInferrer,
}

pub trait InferrerLike<'model> {
	fn figure<'slf>(
		&'slf self,
		expected_ty: Up<'model, TypeParameter<'model>>,
		actual_ty: &Ty<'model>,
	) -> InferResult<'slf, 'model>;
}

//TODO: not pub
pub struct CombinedInferrer<'infer, 'model: 'infer>(
	&'infer Inferrer<'infer, 'model>,
	&'infer Inferrer<'infer, 'model>,
);
impl<'infer, 'model> InferrerLike<'model> for CombinedInferrer<'infer, 'model> {
	fn figure<'slf>(
		&'slf self,
		expected_ty: Up<'model, TypeParameter<'model>>,
		actual_ty: &Ty<'model>,
	) -> InferResult<'slf, 'model> {
		let res = self.0.figure(expected_ty, actual_ty);
		if let InferResult::NotInInferrer = res {
			self.1.figure(expected_ty, actual_ty)
		} else {
			res
		}
	}
}

pub struct Inferrer<'infer, 'model: 'infer> {
	params: &'infer [&'model TypeParameter<'model>],
	// We will gradually fill this in for each param.
	// This will be stored in the InstMethod, so store it in the Model.
	args: &'model [Late<Ty<'model>>],
}
impl<'infer, 'model> Inferrer<'infer, 'model> {
	//TODO: use a const
	pub fn nil() -> Self {
		Inferrer { params: &[], args: &[] }
	}

	pub fn into_ty_args(self) -> &'model [Late<Ty<'model>>] {
		self.args
	}

	pub fn all_inferred(&self) -> bool {
		self.args.iter().all(|a| a.is_initialized())
	}

	pub fn combine(&'infer self, other: &'infer Self) -> CombinedInferrer<'infer, 'model> {
		//TODO: impl InferrerLike<'model> {
		CombinedInferrer(self, other)
	}

	fn new(params: &'infer [&'model TypeParameter<'model>], model_arena: &'model Arena) -> Self {
		let args = model_arena.fill(params.len(), || Late::new());
		Inferrer { params, args }
	}

	fn fill_at(&self, i: usize, ty: Ty<'model>) {
		&self.args[i] <- ty;
	}

	pub fn of_inst_class_and_method(
		inst_class: &InstClass<'model>,
		method: MethodOrImplOrAbstract<'model>,
		infer_arena: &'infer Arena,
		model_arena: &'model Arena,
	) -> Self {
		assert!(method.containing_class() == inst_class.class);
		let &InstClass { class, ty_args } = inst_class;
		let params = infer_arena.write(class.type_parameters.chain(method.type_parameters()));
		let inferrer = Self::new(params, model_arena);
		for (i, ty_arg) in ty_args.iter().enumerate() {
			inferrer.fill_at(i, (*ty_arg).clone())
		}
		inferrer
	}

	pub fn of_static_method(
		method: Up<'model, MethodWithBody<'model>>,
		infer_arena: &'infer Arena,
		arena: &'model Arena,
	) -> Self {
		assert!(method.is_static);
		Self::new(infer_arena.map(method.up_ref().type_parameters(), |p| p), arena)
	}
}
impl<'infer, 'model> InferrerLike<'model> for Inferrer<'infer, 'model> {
	fn figure<'slf>(
		&'slf self,
		expected_ty: Up<'model, TypeParameter<'model>>,
		actual_ty: &Ty<'model>,
	) -> InferResult<'slf, 'model> {
		match self.params.iter().position(|p| expected_ty.ptr_eq(Up(p))) {
			Some(index) => {
				let arg = &self.args[index];
				match arg.try_get() {
					Some(ty) => InferResult::AlreadyHave(ty),
					None => {
						arg <- actual_ty.clone();
						InferResult::JustFilledIn
					}
				}
			}
			None => InferResult::NotInInferrer,
		}
	}
}
impl<'infer, 'model> InstantiatorLike<'model> for Inferrer<'infer, 'model> {
	fn replace(&self, tp: Up<'model, TypeParameter<'model>>) -> Option<Ty<'model>> {
		self.params
			.iter()
			.position(|p| tp.ptr_eq(Up(p)))
			.map(|i| self.args[i].clone())
	}
}
