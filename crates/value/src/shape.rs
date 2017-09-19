use util::arena::{Arena, NoDrop};
use util::dict::{Entry, MutDict};

use model::class::{ClassHead, InstClass};

pub struct ShapeGetter<'model : 'shapes, 'shapes> {
	arena: &'shapes Arena,
	class_to_shape: MutDict<InstClass<'model>, Shape<'shapes>>,
}
impl<'model : 'shapes, 'shapes> ShapeGetter<'model, 'shapes> {
	pub fn new(arena: &'shapes Arena) -> Self {
		ShapeGetter {
			arena,
			class_to_shape: MutDict::new(),
		}
	}

	pub fn get_shape(&mut self, class: &'model InstClass<'model>) -> Shape<'shapes> {
		get_shape_worker(class, &mut self.class_to_shape, &self.arena)
	}
}

#[derive(Copy, Clone)]
pub enum Shape<'shapes> {
	Calculating,
	Primitive,
	Slots(&'shapes SlotsShape<'shapes>),
}
impl<'shapes> NoDrop for Shape<'shapes> {}
impl<'shapes> Shape<'shapes> {
	pub fn assert_slots_shape(self) -> &'shapes SlotsShape<'shapes> {
		match self {
			Shape::Slots(slots) => slots,
			_ => unreachable!(),
		}
	}
}

pub struct SlotsShape<'shapes> {
	// For each word, tells whether it's a pointer or not.
	// Note: We're assuming here that slot = word.
	slots: &'shapes [Shape<'shapes>],
}
impl<'shapes> NoDrop for SlotsShape<'shapes> {}
impl<'shapes> SlotsShape<'shapes> {
	pub fn size_in_words(&self) -> usize {
		self.slots.len()
	}
}

// Use a worker fn to make clear to rust that use of `class_to_shape` and `arena` are separate.
fn get_shape_worker<'model, 'shapes>(
	inst_class: &'model InstClass<'model>,
	class_to_shape: &mut MutDict<InstClass<'model>, Shape<'shapes>>,
	arena: &'shapes Arena
) -> Shape<'shapes> {
	//TODO: way to do lookup without clone?
	match class_to_shape.entry(inst_class.clone()) {
		Entry::Occupied(e) => return *e.get(),
		Entry::Vacant(v) => { v.insert(Shape::Calculating); },
	}

	let &InstClass { class, ty_args } = inst_class;
	if !ty_args.is_empty() {
		unimplemented!()
	}
	let res = match *class.head {
		ClassHead::Builtin => Shape::Primitive, //TODO: not true for arrays
		ClassHead::Abstract(_, _) => unimplemented!(), //TODO: tag + pointer
		ClassHead::Static => unreachable!(),
		ClassHead::Slots(_, slots) => {
			//TODO: would be nice to just use `self.arena.map`, but lifetime woes...
			let mut slot_shapes = arena.exact_len_builder(slots.len());
			for slot in slots {
				// Ty shouldn't be Bogus or we should fail compiling.
				// Ty shouldn't be a Param because it's coming from an InstClass.
				&mut slot_shapes <- get_shape_worker(&slot.ty.assert_plain().inst_class, class_to_shape, arena);
			}
			Shape::Slots(arena <- SlotsShape {
				slots: slot_shapes.finish()
			})
		}
	};
	class_to_shape.change(&inst_class) <- res;
	res
}

