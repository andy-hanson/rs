use std::borrow::Borrow;

use serde::Serialize;
use serde_json::to_string as to_json_string;

use compile::EXTENSION;

use model::module::ModuleOrFail;

use util::arena::Arena;
use util::dict::MutDict;
use util::file_utils::{write_file, write_file_and_ensure_directory};
use util::path::Path;

use super::test_failure::{io_result_to_result, TestFailure, TestResult};

pub enum BaselinesUpdate {
	None,
	Create,
	Change,
}

pub struct Baselines<'a: 'expected, 'expected> {
	pub arena: &'a Arena,
	pub baselines_directory: Path<'a>,
	pub expected: &'expected mut MutDict<Path<'a>, &'a [u8]>,
	pub update_baselines: BaselinesUpdate,
}
impl<'a, 'expected> Baselines<'a, 'expected> {
	pub fn assert_baseline<T: Serialize>(
		&mut self,
		module_or_fail: ModuleOrFail,
		extension: &[u8],
		actual: &T,
	) -> TestResult<'a, ()> {
		let actual_str = to_json(actual, self.arena);
		self.assert_baseline_worker(get_module_path_without_extension(module_or_fail), extension, actual_str)
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
				} else {
					match self.update_baselines {
						BaselinesUpdate::None | BaselinesUpdate::Create =>
							Err(TestFailure::UnexpectedOutput { actual, expected }),
						BaselinesUpdate::Change =>
							io_result_to_result(write_file(full_baseline_path, actual)),
					}
				},
			None =>
				match self.update_baselines {
					BaselinesUpdate::None =>
						Err(TestFailure::NoSuchBaseline(full_baseline_path)),
					BaselinesUpdate::Create | BaselinesUpdate::Change =>
						io_result_to_result(write_file_and_ensure_directory(full_baseline_path, actual)),
				},
		}
	}
}

fn get_module_path_without_extension(module_or_fail: ModuleOrFail) -> Path {
	let source = module_or_fail.source().assert_normal();
	source.full_path.without_extension(EXTENSION)
}

fn to_json<'out, T: Serialize>(value: &T, arena: &'out Arena) -> &'out [u8] {
	//TODO:PERF directly serialize to arena
	arena.copy_slice(to_json_string(value).unwrap().as_bytes())
}
