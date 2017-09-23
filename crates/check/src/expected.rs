use std::cell::UnsafeCell;

use util::arena::NoDrop;

use model::ty::Ty;

use super::inferrer::Inferrer;

#[derive(Copy, Clone)]
pub struct Expected<'infer, 'model: 'infer> {
	pub in_tail_call_position: bool,
	pub expected_ty: &'infer UnsafeCell<Option<Ty<'model>>>,
	pub inferrer: &'infer Inferrer<'infer, 'model>,
}
impl<'infer, 'model> NoDrop for Expected<'infer, 'model> {}
impl<'infer, 'model> Expected<'infer, 'model> {
	pub fn inferred(self) -> &'infer Ty<'model> {
		assert!(self.inferrer.all_inferred());
		unsafe { self.expected_ty.get().as_ref().unwrap().as_ref().unwrap() }
	}
}
