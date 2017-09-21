use util::arena::NoDrop;
use util::show::{Show, Shower};

use model::method::MethodOrImpl;

pub type EmitResult<'model, T> = Result<T, EmitError<'model>>;

pub enum EmitError<'model> {
	MissingBuiltin(MethodOrImpl<'model>),
	WrongBuiltinArity { expected: MethodOrImpl<'model>, actual: u8 },
}
impl<'model> NoDrop for EmitError<'model> {}
impl<'e, 'model> Show for &'e EmitError<'model> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			EmitError::MissingBuiltin(method) => {
				//TODO: share code for printing a method along with its class
				s.add("Missing builtin impl for method '")?
					.add(method)?
					.add("'.")?;
			}
			EmitError::WrongBuiltinArity { expected, actual } => {
				s.add("Expected method '")?
					.add(expected)?
					.add("' to have arity ")?
					.add(expected.arity())?
					.add(", but builtin impl has arity ")?
					.add(actual)?;
			}
		}
		Ok(())
	}
}
