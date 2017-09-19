use util::arena::Arena;

use model::builtins::BuiltinsOwn;
use model::ty::Ty;

use super::heap::Heap;
use super::value::{Value, ValueInner};
use super::shape::{Shape, ShapeGetter};
use super::stack::ValueStack;

pub struct ValueCtx<'model : 'value, 'value> {
	pub(crate) builtins: &'model BuiltinsOwn<'model>,
	stack: ValueStack<'model, 'value>,
	heap: Heap<'value>,
	shapes: ShapeGetter<'model, 'value>,
}
impl<'model, 'value> ValueCtx<'model, 'value> {
	pub fn new(builtins: &'model BuiltinsOwn<'model>, arena: &'value Arena) -> Self {
		ValueCtx { builtins, stack: ValueStack::new(), heap: Heap::new(arena), shapes: ShapeGetter::new(arena) }
	}

	pub fn depth(&self) -> usize { self.stack.depth() }
	pub fn pop(&mut self) -> Value<'model, 'value> { self.stack.pop() }
	pub fn push(&mut self, value: Value<'model, 'value>) { self.stack.push(value) }
	pub fn fetch(&mut self, by: u8) { self.stack.fetch(by) }
	pub fn un_let(&mut self, n: u8) { self.stack.un_let(n) }

	fn get_shape(&mut self, ty: &'model Ty<'model>) -> Shape<'value> {
		self.shapes.get_shape(&ty.assert_plain().inst_class)
	}

	// Pops values off the stack and stores them on the heap. Then puts that onto the stack.
	pub fn pop_slots_and_push_new_value(&mut self, ty: &'model Ty<'model>, n: usize) {
		let shape = self.get_shape(ty);
		assert_eq!(shape.assert_slots_shape().size_in_words(), n);

		let inner = self.heap.new_slots(self.stack.get_slice_of_top(n));
		self.stack.drop(n);
		let value = Value { value: inner, ty };
		self.stack.push(value)
	}

	pub fn void(&self) -> Value<'model, 'value> {
		Value { value: ValueInner { void: () }, ty: &self.builtins.void }
	}

	pub fn bool(&self, bool: bool) -> Value<'model, 'value> {
		Value { value: ValueInner { bool }, ty: &self.builtins.bool }
	}

	pub fn nat(&self, nat: u32) -> Value<'model, 'value> {
		Value { value: ValueInner { nat }, ty: &self.builtins.nat }
	}

	pub fn int(&self, int: i32) -> Value<'model, 'value> {
		Value { value: ValueInner { int }, ty: &self.builtins.int }
	}

	pub fn float(&self, float: f64) -> Value<'model, 'value> {
		Value { value: ValueInner { float }, ty: &self.builtins.float }
	}
}
