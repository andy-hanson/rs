use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::{IntoIter, Iter, Values};
use std::hash::Hash;
use std::iter::FromIterator;

use super::arena::{NoDrop, PointerPlace};
use super::iter::KnownLen;

pub use std::collections::hash_map::Entry;

// TODO:PERF don't use HashMap
// Immutable hash map
pub struct Dict<K: Hash + Eq, V>(HashMap<K, V>);
impl<K: Hash + Eq, V> Dict<K, V> {
	pub fn of(keysvals: Vec<(K, V)>) -> Self {
		let mut b = MutDict::new();
		for (k, v) in keysvals {
			b.add(k) <- v
		}
		b.freeze()
	}

	pub fn iter(&self) -> Iter<K, V> {
		self.0.iter()
	}

	pub fn from_iterator<T: IntoIterator<Item = (K, V)>>(i: T) -> Self {
		MutDict::from_iterator(i).freeze()
	}

	pub fn get<Q: Hash + Eq>(&self, k: Q) -> Option<&V>
	where
		K: Borrow<Q>,
	{
		self.0.get(&k)
	}
}

pub struct MutDict<K: Hash + Eq, V>(HashMap<K, V>);
impl<K: Hash + Eq, V> MutDict<K, V> {
	pub fn new() -> Self {
		MutDict(HashMap::new())
	}

	pub fn entry(&mut self, key: K) -> Entry<K, V> {
		self.0.entry(key)
	}

	//pub fn get_or_add<F : Fn() -> V>(&mut self, key: &K, f: F) -> V where K : Clone {
 //	let entry = self.0.entry(key.clone());
 //	unimplemented!()
 //}

	pub fn from_iterator<T: IntoIterator<Item = (K, V)>>(i: T) -> Self {
		MutDict(HashMap::from_iter(i))
	}

	pub fn values(&self) -> Values<K, V> {
		self.0.values()
	}

	pub fn change<Q: Hash + Eq>(&mut self, key: &Q) -> PointerPlace<V>
	where
		K: Borrow<Q>,
		V: NoDrop,
	{
		PointerPlace::new(self.0.get_mut(key).unwrap())
	}

	pub fn add(&mut self, key: K) -> Entry<K, V> {
		let entry = self.0.entry(key);
		if let Entry::Occupied(_) = entry {
			unreachable!()
		}
		entry
	}

	pub fn try_extract<Q: Hash + Eq>(&mut self, key: Q) -> Option<V>
	where
		K: Borrow<Q>,
	{
		self.0.remove(&key)
	}

	pub fn has_key<Q: Hash + Eq>(&self, k: Q) -> bool
	where
		K: Borrow<Q>,
	{
		self.0.contains_key(&k)
	}

	pub fn get<Q: Hash + Eq>(&self, k: Q) -> Option<&V>
	where
		K: Borrow<Q>,
	{
		self.0.get(&k)
	}

	pub fn freeze(self) -> Dict<K, V> {
		Dict(self.0)
	}

	pub fn map_values<V2, F: Fn(V) -> V2>(self, f: F) -> MutDict<K, V2> {
		let mut d = MutDict::<K, V2>::new();
		for (k, v) in self.move_into_iter() {
			d.add(k) <- f(v)
		}
		d
	}

	fn move_into_iter(self) -> IntoIter<K, V> {
		self.0.into_iter()
	}
}
impl<'a, K: Hash + Eq, V> IntoIterator for &'a MutDict<K, V> {
	type Item = (&'a K, &'a V);
	type IntoIter = Iter<'a, K, V>;

	fn into_iter(self) -> Self::IntoIter {
		self.0.iter()
	}
}
impl<'a, K: Hash + Eq, V> KnownLen for &'a MutDict<K, V> {
	fn len(self) -> usize {
		self.0.len()
	}

	fn is_empty(self) -> bool {
		self.0.is_empty()
	}
}

pub struct MutSet<K: Hash + Eq>(MutDict<K, ()>);
impl<K: Hash + Eq> MutSet<K> {
	pub fn new() -> MutSet<K> {
		MutSet(MutDict::new())
	}

	pub fn add(&mut self, value: K) {
		self.0.add(value) <- ()
	}

	pub fn has<Q: Hash + Eq>(&self, value: Q) -> bool
	where
		K: Borrow<Q>,
	{
		self.0.has_key(value)
	}
}
