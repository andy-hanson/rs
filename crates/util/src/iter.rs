use std::iter::{Chain, Enumerate, Iterator, Zip};
use std::mem::replace;

// Not using ExactSizeIterator because that requires the iterator to keep track of its length;
// We only need to know that up-front.
pub trait KnownLen: IntoIterator
where
	Self: Copy + Sized,
{
	fn len(self) -> usize;
	fn is_empty(self) -> bool;

	//TODO: shouldn't need where clause...
	fn chain<I : KnownLen<Item = Self::Item>>(self, other: I) -> ChainKnownLen<Self::Item, Self, I> where Self::Item : Copy { //TODO: impl KnownLen<Item=Self::Item> {
		ChainKnownLen(self, other)
	}

	fn each_equals<I: KnownLen<Item = Self::Item>, F: Fn(Self::Item, Self::Item) -> bool>(
		self,
		other: I,
		f: F,
	) -> bool {
		self.each_corresponds(other, f)
	}

	fn each_corresponds<U, I: KnownLen<Item = U>, F: Fn(Self::Item, U) -> bool>(
		self,
		other: I,
		f: F,
	) -> bool {
		self.len() == other.len() &&
			self.into_iter()
				.zip(other.into_iter())
				.all(|(a, b)| f(a, b))
	}

	fn enumerate(self) -> EnumeratedKnownLen<Self> { //TODO: impl KnownLen<Self::Item> {
		EnumeratedKnownLen(self)
	}

	fn zip<U, I: KnownLen<Item = U>>(self, other: I) -> ZipKnownLen<Self, I> {//TODO: impl KnownLen<(Self::Item, U)> {
		assert_eq!(self.len(), other.len());
		ZipKnownLen(self, other)
	}
}
impl<'a, T> KnownLen for &'a [T] {
	fn len(self) -> usize {
		self.len()
	}

	fn is_empty(self) -> bool {
		self.is_empty()
	}
}

//TODO: shouldn't need T : Copy since we don't ever contain a T...
#[derive(Copy, Clone)]
pub struct ChainKnownLen<T : Copy, A : KnownLen<Item=T>, B : KnownLen<Item=T>>(A, B);
//impl<T, A : KnownLen<Item=T>, B : KnownLen<Item=T>> Clone for ChainKnownLen<T, A, B> {
//	fn clone(&self) -> Self {
//		ChainKnownLen(self.0.clone(), self.1.clone())
//	}
//}
impl<T : Copy, A : KnownLen<Item=T>, B : KnownLen<Item=T>> KnownLen for ChainKnownLen<T, A, B> {
	fn len(self) -> usize {
		self.0.len() + self.1.len()
	}

	fn is_empty(self) -> bool {
		self.0.is_empty() && self.1.is_empty()
	}
}
impl<T : Copy, A : KnownLen<Item=T>, B : KnownLen<Item=T>> IntoIterator for ChainKnownLen<T, A, B> {
	type Item = T;
	type IntoIter = Chain<A::IntoIter, B::IntoIter>;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter().chain(self.1)
	}
}

#[derive(Copy, Clone)]
pub struct EnumeratedKnownLen<K: KnownLen>(K);
impl<K: KnownLen> KnownLen for EnumeratedKnownLen<K> {
	fn len(self) -> usize {
		self.0.len()
	}

	fn is_empty(self) -> bool {
		self.0.is_empty()
	}
}
impl<K: KnownLen> IntoIterator for EnumeratedKnownLen<K> {
	type Item = <Self::IntoIter as IntoIterator>::Item;
	type IntoIter = Enumerate<K::IntoIter>;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter().enumerate()
	}
}

#[derive(Copy, Clone)]
pub struct ZipKnownLen<K1: KnownLen, K2: KnownLen>(K1, K2);
impl<K1: KnownLen, K2: KnownLen> KnownLen for ZipKnownLen<K1, K2> {
	fn len(self) -> usize {
		let l1 = self.0.len();
		let l2 = self.1.len();
		assert_eq!(l1, l2);
		l1
	}

	fn is_empty(self) -> bool {
		self.0.is_empty()
	}
}
impl<K1: KnownLen, K2: KnownLen> IntoIterator for ZipKnownLen<K1, K2> {
	type Item = <Self::IntoIter as IntoIterator>::Item;
	type IntoIter = Zip<K1::IntoIter, K2::IntoIter>;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter().zip(self.1.into_iter())
	}
}

#[derive(Copy, Clone)]
pub struct OptionIter<T>(pub Option<T>);
impl<T> Iterator for OptionIter<T> {
	type Item = T;

	fn next(&mut self) -> Option<T> {
		replace(&mut self.0, None)
	}
}
impl<T: Copy> KnownLen for OptionIter<T> {
	fn len(self) -> usize {
		match self.0 {
			Some(_) => 1,
			None => 0,
		}
	}

	fn is_empty(self) -> bool {
		self.0.is_none()
	}
}

// Used by serializers.
pub fn slice_is_empty<T>(slice: &&[T]) -> bool {
	slice.is_empty()
}
