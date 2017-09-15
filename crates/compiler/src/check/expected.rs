use util::arena::NoDrop;

use model::ty::Ty;

pub enum Expected<'model> {
	/* Return is identical to SubTypeOf, but marks that we're in a tail call position. */
	Return(Ty<'model>),
	SubTypeOf(Ty<'model>),
	/* Expected should always be passed by `&mut`, so that inferred types can be inserted here. */
	Infer(Option<Ty<'model>>),
}
impl<'model> NoDrop for Expected<'model> {}
impl<'model> Expected<'model> {
	pub fn inferred_ty(&self) -> &Ty<'model> {
		self.current_expected_ty().unwrap()
	}

	pub fn current_expected_ty(&self) -> Option<&Ty<'model>> {
		match *self {
			Expected::Return(ref ty) | Expected::SubTypeOf(ref ty) => Some(ty),
			Expected::Infer(ref ty_op) => ty_op.as_ref(),
		}
	}

	pub fn in_tail_call_position(&self) -> bool {
		match *self {
			Expected::Return(_) => true,
			_ => false,
		}
	}
}
