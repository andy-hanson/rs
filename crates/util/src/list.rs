use std::cell::{Cell, UnsafeCell};
use std::mem::uninitialized;
use std::ops::Placer;

use super::arena::{Arena, NoDrop, PointerPlace};
use super::iter::KnownLen;

pub struct List<'a, T: NoDrop + 'a>(Option<&'a ListHead<'a, T>>);
impl<'a, T: NoDrop + 'a> List<'a, T> {
	pub fn empty() -> Self {
		List(None)
	}

	pub fn single(value: T, arena: &'a Arena) -> Self {
		List(Some(
			arena <- ListHead {
			len: Cell::new(1),
			node: ListNode {
				value: UnsafeCell::new(value),
				next: Cell::new(None)
			}
		},
		))
	}

	pub fn iter(&self) -> ListIter<'a, T> {
		ListIter(self.0.map(|head| &head.node))
	}
}
impl<'a, T: NoDrop + 'a> NoDrop for List<'a, T> {}
impl<'a, T: NoDrop + 'a> Copy for List<'a, T> {}
#[allow(expl_impl_clone_on_copy)] // False positive?
impl<'a, T: NoDrop + 'a> Clone for List<'a, T> {
	fn clone(&self) -> Self {
		List(self.0)
	}
}
impl<'a, T: NoDrop + 'a> IntoIterator for List<'a, T> {
	type Item = &'a T;
	type IntoIter = ListIter<'a, T>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}
impl<'a, T: NoDrop> KnownLen for List<'a, T> {
	fn len(self) -> usize {
		match self.0 {
			Some(head) => head.len.get(),
			None => 0,
		}
	}

	fn is_empty(self) -> bool {
		self.0.is_none()
	}
}

struct ListHead<'a, T: 'a> {
	len: Cell<usize>, // Non-zero or else we would not have allocated the head.
	node: ListNode<'a, T>,
}
impl<'a, T> NoDrop for ListHead<'a, T> {}

struct ListNode<'a, T: 'a> {
	value: UnsafeCell<T>,
	next: Cell<Option<&'a ListNode<'a, T>>>,
}
impl<'a, T: NoDrop> NoDrop for ListNode<'a, T> {}

pub struct ListIter<'a, T: 'a>(Option<&'a ListNode<'a, T>>);
impl<'a, T: 'a> Iterator for ListIter<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<&'a T> {
		self.0.map(|n| {
			let value = &n.value;
			self.0 = n.next.get();
			unsafe { value.get().as_ref().unwrap() }
		})
	}
}

pub struct ListBuilder<'a, T: 'a + Sized + NoDrop> {
	arena: &'a Arena,
	data: Option<(&'a ListHead<'a, T>, &'a ListNode<'a, T>)>,
}
impl<'a, T: 'a + Sized + NoDrop> ListBuilder<'a, T> {
	pub fn new(arena: &'a Arena) -> Self {
		ListBuilder { arena, data: None }
	}

	pub fn finish(self) -> List<'a, T> {
		List(self.data.map(|(first, _)| &*first))
	}
}
impl<'l, 'a, T: 'a + Sized + NoDrop> Placer<T> for &'l mut ListBuilder<'a, T> {
	type Place = PointerPlace<'a, T>;

	fn make_place(self) -> Self::Place {
		let (head, last): (&'a ListHead<'a, T>, &'a ListNode<'a, T>) = match self.data {
			Some((head, last)) => {
				head.len.set(head.len.get() + 1);
				let new_last = self.arena <- ListNode {
					value: UnsafeCell::new(unsafe { uninitialized() }),
					next: Cell::new(None),
				};
				last.next.set(Some(new_last));
				(head, new_last)
			}
			None => {
				let head = self.arena <- ListHead {
					len: Cell::new(1),
					node: ListNode {
						value: UnsafeCell::new(unsafe { uninitialized() }),
						next: Cell::new(None),
					}
				};
				(head, &head.node)
			}
		};
		self.data = Some((head, last));
		PointerPlace::new(last.value.get())
	}
}
