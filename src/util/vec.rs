pub fn find_index<T, F: Fn(&T) -> bool>(v: &Vec<T>, f: F) -> Option<usize> {
	for i in 0..v.len() {
		if f(&v[i]) {
			return Some(i)
		}
	}
	None
}
