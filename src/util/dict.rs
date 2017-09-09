use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::{IntoIter, Iter, Values};
use std::hash::Hash;
use std::iter::FromIterator;

use super::arena::Arena;

// TODO:PERF don't use HashMap
// Immutable hash map
pub struct Dict<K: Hash + Eq, V>(HashMap<K, V>);
impl<K: Hash + Eq, V> Dict<K, V> {
	pub fn of(keysvals: Vec<(K, V)>) -> Self {
		let mut b = MutDict::new();
		for (k, v) in keysvals {
			b.add(k, v)
		}
		b.freeze()
	}

	pub fn iter(&self) -> Iter<K, V> {
		self.0.iter()
	}

	pub fn from_iterator<T: IntoIterator<Item = (K, V)>>(i: T) -> Self {
		MutDict::from_iterator(i).freeze()
	}

	pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
	where
		K: Borrow<Q>,
		Q: Hash + Eq,
	{
		self.0.get(k)
	}
}

pub struct MutDict<K: Hash + Eq, V>(HashMap<K, V>);
impl<K: Hash + Eq, V> MutDict<K, V> {
	pub fn new() -> Self {
		MutDict(HashMap::new())
	}

	pub fn from_iterator<T: IntoIterator<Item = (K, V)>>(i: T) -> Self {
		MutDict(HashMap::from_iter(i))
	}

	pub fn values(&self) -> Values<K, V> {
		self.0.values()
	}

	pub fn any(&self) -> bool {
		!self.0.is_empty()
	}

	pub fn add(&mut self, key: K, value: V) {
		if let Some(_old) = self.0.insert(key, value) {
			panic!()
		}
	}

	pub fn try_extract<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
	where
		K: Borrow<Q>,
		Q: Hash + Eq,
	{
		self.0.remove(key)
	}

	pub fn change<Q: ?Sized>(&mut self, key: &Q, new_value: V)
	where
		K: Borrow<Q>,
		Q: Hash + Eq,
	{
		*self.0.get_mut(key).unwrap() = new_value;
	}

	pub fn has_key<Q: ?Sized>(&self, k: &Q) -> bool
	where
		K: Borrow<Q>,
		Q: Hash + Eq,
	{
		self.0.contains_key(k)
	}

	pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
	where
		K: Borrow<Q>,
		Q: Hash + Eq,
	{
		self.0.get(k)
	}

	pub fn freeze(self) -> Dict<K, V> {
		Dict(self.0)
	}

	pub fn map_values<V2, F: Fn(V) -> V2>(self, f: F) -> MutDict<K, V2> {
		let mut d = MutDict::<K, V2>::new();
		for (k, v) in self.move_into_iter() {
			d.add(k, f(v))
		}
		d
	}

	fn move_into_iter(self) -> IntoIter<K, V> {
		self.0.into_iter()
	}

	pub fn into_keys<'out>(self, arena: &'out Arena) -> &'out [K] {
		arena.map_from(self.0.len(), self.move_into_iter(), |(k, _)| k)
	}
}

pub struct MutSet<K: Hash + Eq>(MutDict<K, ()>);
impl<K: Hash + Eq> MutSet<K> {
	pub fn new() -> MutSet<K> {
		MutSet(MutDict::new())
	}

	pub fn add(&mut self, value: K) {
		self.0.add(value, ())
	}

	pub fn has<Q: ?Sized>(&self, value: &Q) -> bool
	where
		K: Borrow<Q>,
		Q: Hash + Eq,
	{
		self.0.has_key(value)
	}
}
