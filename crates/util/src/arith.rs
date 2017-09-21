use std::{isize, usize, i32, u32, u8};

pub fn mid(a: usize, b: usize) -> usize {
	(a + b) / 2
}

pub fn u8_to_usize(u: u8) -> usize {
	u as usize
}

pub fn u32_to_usize(u: u32) -> usize {
	u as usize
}

pub fn u32_to_i32(u: u32) -> i32 {
	assert!(u < i32::MAX as u32);
	u as i32
}

pub fn u64_to_usize(u: u64) -> usize {
	assert!(u < usize::MAX as u64);
	u as usize
}

pub fn isize_to_usize(i: isize) -> usize {
	assert!(i >= 0);
	i as usize
}

pub fn usize_to_isize(u: usize) -> isize {
	assert!(u <= isize::MAX as usize);
	u as isize
}

pub fn usize_to_u32(u: usize) -> u32 {
	assert!(u <= u32::MAX as usize);
	u as u32
}

pub fn u8_add_mut(a: &mut u8, b: u8) {
	*a = u8_add(*a, b)
}

pub fn u8_add(a: u8, b: u8) -> u8 {
	let (result, overflow) = a.overflowing_add(b);
	assert!(!overflow);
	result
}

pub fn uadd(a: usize, b: usize) -> usize {
	let (result, overflow) = a.overflowing_add(b);
	assert!(!overflow);
	result
}

pub fn u8_sub_mut(a: &mut u8, b: u8) {
	*a = u8_sub(*a, b)
}

pub fn u8_sub(a: u8, b: u8) -> u8 {
	let (result, overflow) = a.overflowing_sub(b);
	assert!(!overflow);
	result
}

pub fn usub(a: usize, b: usize) -> usize {
	let (result, overflow) = a.overflowing_sub(b);
	assert!(!overflow);
	result
}

pub fn usize_to_u8(u: usize) -> u8 {
	assert!(u <= u8::MAX as usize);
	u as u8
}
