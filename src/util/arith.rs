pub fn usub(a: u32, b: u32) -> u32 {
	let (result, overflow) = a.overflowing_sub(b);
	assert!(!overflow);
	result
}
