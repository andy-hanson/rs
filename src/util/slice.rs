pub fn find_index<T, F: Fn(&T) -> bool>(v: &[T], f: F) -> Option<usize> {
	for (i, x) in v.iter().enumerate() {
		if f(x) {
			return Some(i)
		}
	}
	None
}
