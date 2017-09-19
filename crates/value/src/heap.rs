use util::arena::Arena;

use super::value::{Value, ValueInner};

// Public from this module but private to the crate. ValueCtx handles everything.
pub(crate) struct Heap<'value> {
	arena: &'value Arena,
}
impl<'value> Heap<'value> {
	pub fn new(arena: &'value Arena) -> Self {
		Heap { arena }
	}

	//TODO: this should return Option because we might need to start a GC instead...
	pub fn new_slots<'model : 'value>(&self, slice: &[Value<'model, 'value>]) -> ValueInner<'model, 'value> {
		ValueInner { ptr: self.arena.copy_slice(slice) }
	}
}
