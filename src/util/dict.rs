use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::{IntoIter, Iter};
use std::hash::Hash;
use std::iter::FromIterator;

// TODO:PERF don't use HashMap

// Immutable hash map
pub struct Dict<K: Hash + Eq, V>(HashMap<K, V>);
impl<K: Hash + Eq, V> Dict<K, V> {
	pub fn of(keysvals: Vec<(K, V)>) -> Dict<K, V> {
		let mut b = MutDict::new();
		for (k, v) in keysvals {
			b.add(k, v)
		}
		b.freeze()
	}

	pub fn iter(&self) -> Iter<K, V> {
		self.0.iter()
	}

	pub fn from_iterator<T: IntoIterator<Item = (K, V)>>(i: T) -> Dict<K, V> {
		Dict(HashMap::from_iter(i))
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
	pub fn new() -> MutDict<K, V> {
		MutDict(HashMap::new())
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
}
