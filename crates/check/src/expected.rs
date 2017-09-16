use std::cell::UnsafeCell;

use util::arena::NoDrop;

use model::ty::Ty;

#[derive(Copy, Clone)]
pub enum Expected<'ty, 'model : 'ty> {
	/* Return is identical to SubTypeOf, but marks that we're in a tail call position. */
	Return(&'ty Ty<'model>),
	SubTypeOf(&'ty Ty<'model>),
	/* Expected should always be passed by `&mut`, so that inferred types can be inserted here. */
	Infer(&'ty UnsafeCell<Option<Ty<'model>>>),
}
impl<'ty, 'model> NoDrop for Expected<'ty, 'model> {}
impl<'ty, 'model> Expected<'ty, 'model> {
	pub fn inferred_ty(&self) -> &Ty<'model> {
		self.current_expected_ty().unwrap()
	}

	pub fn current_expected_ty(&self) -> Option<&Ty<'model>> {
		match *self {
			Expected::Return(ty) | Expected::SubTypeOf(ty) => Some(ty),
			Expected::Infer(cell) => unsafe { cell.get().as_ref() } .unwrap().as_ref(),
		}
	}

	pub fn in_tail_call_position(&self) -> bool {
		match *self {
			Expected::Return(_) => true,
			_ => false,
		}
	}
}
