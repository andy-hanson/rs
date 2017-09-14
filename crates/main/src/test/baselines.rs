use std::borrow::Borrow;

use serde::Serialize;
use serde_json::to_string as to_json_string;

use util::arena::Arena;
use util::dict::MutDict;
use util::file_utils::{write_file, write_file_and_ensure_directory};
use util::path::Path;

use super::test_failure::{io_result_to_result, TestFailure, TestResult};

pub struct Baselines<'a: 'expected, 'expected> {
	pub arena: &'a Arena,
	pub baselines_directory: Path<'a>,
	pub expected: &'expected mut MutDict<Path<'a>, &'a [u8]>,
	pub update_baselines: bool,
}
impl<'a, 'expected> Baselines<'a, 'expected> {
	pub fn assert_baseline<T: Serialize>(
		&mut self,
		module_path_without_extension: Path,
		extension: &[u8],
		actual: &T,
	) -> TestResult<'a, ()> {
		let actual_str = to_json(actual, self.arena);
		self.assert_baseline_worker(module_path_without_extension, extension, actual_str)
	}

	fn assert_baseline_worker(
		&mut self,
		module_path_without_extension: Path,
		extension: &[u8],
		actual: &'a [u8],
	) -> TestResult<'a, ()> {
		let baseline_path = module_path_without_extension.add_extension(extension, self.arena);
		let full_baseline_path = Path::resolve_with_root(self.baselines_directory, baseline_path, self.arena);

		match self.expected.try_extract::<[u8]>(baseline_path.borrow()) {
			Some(expected) =>
				if actual == expected {
					Ok(())
				} else if self.update_baselines {
					io_result_to_result(write_file(full_baseline_path, actual))
				} else {
					Err(TestFailure::UnexpectedOutput { actual, expected })
				},
			None => {
				// This baseline didn't exist before.
				if self.update_baselines {
					io_result_to_result(write_file_and_ensure_directory(full_baseline_path, actual))
				} else {
					Err(TestFailure::NoSuchBaseline(full_baseline_path))
				}
			}
		}
	}
}

fn to_json<'out, T: Serialize>(value: &T, arena: &'out Arena) -> &'out [u8] {
	//TODO:PERF (also sanity)
	//to_json_string(actual).unwrap().into_boxed_str().as_bytes(),
	arena.copy_slice(to_json_string(value).unwrap().as_bytes())
}
