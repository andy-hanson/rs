pub struct UnsafeSync<T>(pub T);
unsafe impl<T> Sync for UnsafeSync<T> {}
impl<T> UnsafeSync<T> {
	pub fn get<'a>(&'a self) -> &'a T {
		&self.0
	}
}
