use serde::Serialize;
use serde_json::to_string as to_json_string;

use util::arr::{Arr, SliceOps};
use util::dict::{Dict, MutDict};
use util::file_utils::{IoError, write_file, write_file_and_ensure_directory, read_files_in_directory_recursive_if_exists};
use util::path::Path;

mod test_document_provider;
use super::compiler::{compile, CompileResult, CompiledProgram, EXTENSION};
use super::model::module::{Module, PtrModuleOrFail, OwnModuleOrFail};

use self::test_document_provider::{ExpectedDiagnostic, TestDocumentProvider};

const TEST_DIR: &'static str = "tests";
lazy_static! {
	static ref CASES_ROOT_DIR: Path = Path::from_parts(arr![TEST_DIR, "cases",]);
	static ref BASELINES_ROOT_DIR: Path = Path::from_parts(arr![TEST_DIR, "builtins",]);
}

enum TestFailure {
	NoIndex,
	IoError(IoError),
	ExtraBaselines(Arr<Path>),
	ExpectedDiagnostics(Dict<Path, Arr<ExpectedDiagnostic>>),
	NoSuchBaseline(Path),
	UnexpectedOutput {
		actual: Arr<u8>,
		expected: Arr<u8>,
	}
}
type TestResult<T> = ::std::result::Result<T, TestFailure>;

fn test_single(test_path: &Path, update_baselines: bool) -> TestResult<()> {
	//let test_data = run_compiler_test(Path::from_parts(arr![test_name]));
	//run_test(test-
	let test_directory = Path::resolve_with_root(&CASES_ROOT_DIR, test_path);
	let baselines_directory = Path::resolve_with_root(&BASELINES_ROOT_DIR, test_path);

	let document_provider = TestDocumentProvider::new(test_directory);
	let compile_result = io_result_to_result(compile(Path::empty(), &document_provider, None))?;
	let (program, root) = match compile_result {
		CompileResult::RootMissing => return Err(TestFailure::NoIndex),
		CompileResult::RootFound(program, root) => (program, root),
	};

	let mut expected_diagnostics = document_provider.get_expected_diagnostics();
	let mut expected_baselines = read_files_in_directory_recursive_if_exists(&baselines_directory);

	match root {
		PtrModuleOrFail::Module(m) =>
			if any_diagnostics(&m) {
				test_with_diagnostics(
					&baselines_directory,
					&program,
					&mut expected_baselines,
					&mut expected_diagnostics,
					update_baselines,
				)
			} else {
				if expected_diagnostics.any() {
					return Err(TestFailure::ExpectedDiagnostics(expected_diagnostics.freeze()))
				}
				test_with_no_diagnostics(
					test_path,
					&baselines_directory,
					&program,
					&m,
					&mut expected_baselines,
					update_baselines,
				)
			},
		PtrModuleOrFail::Fail(_) =>
			test_with_diagnostics(
				&baselines_directory,
				&program,
				&mut expected_baselines,
				&mut expected_diagnostics,
				update_baselines,
			),
	}?;

	unused!(update_baselines);
	if expected_baselines.any() {
		Err(TestFailure::ExtraBaselines(expected_baselines.into_keys()))
	} else {
		Ok(())
	}
}

lazy_static! {
	static ref EXT_MODEL: Arr<u8> = Arr::copy_from_str(".model");
}

fn test_with_diagnostics(
	baselines_directory: &Path,
	program: &CompiledProgram,
	expected_baselines: &mut MutDict<Path, Arr<u8>>,
	expected_diagnostics_by_path: &mut MutDict<Path, Arr<ExpectedDiagnostic>>,
	update_baselines: bool,
) -> TestResult<()> {
	for module_or_fail in program.modules.values() {
		let source = module_or_fail.source().unwrap();
		let module_full_path = source.full_path();
		let module_path = module_full_path.without_extension(&EXTENSION);
		unused!(baselines_directory, expected_baselines, expected_diagnostics_by_path, module_path);

		if let OwnModuleOrFail::Module(ref module) = *module_or_fail {
			assert_baseline(
				baselines_directory,
				&module_path,
				&EXT_MODEL,
				&module.class,
				expected_baselines,
				update_baselines)?
		}

		let text = &source.document.text;
		let actual_diagnostics = module_or_fail.diagnostics();
		unused!(text, actual_diagnostics);
		unimplemented!()
	}
	unimplemented!()
}

fn assert_baseline<T : Serialize>(
	test_directory: &Path,
	module_path: &Path,
	extension: &[u8],
	actual: &T,
	baselines: &mut MutDict<Path, Arr<u8>>,
	update_baselines: bool) -> TestResult<()> {
	assert_baseline_worker(
		test_directory,
		module_path,
		extension,
		Arr::from_string(to_json_string(actual).unwrap()),
		baselines,
		update_baselines)
}

fn assert_baseline_worker(
	test_directory: &Path,
	module_path: &Path,
	extension: &[u8],
	actual: Arr<u8>,
	baselines: &mut MutDict<Path, Arr<u8>>,
	update_baselines: bool) -> TestResult<()> {

	let baseline_path = module_path.add_extension(extension);
	let full_baseline_path = Path::resolve_with_root(test_directory, &baseline_path);

	match baselines.try_extract(&baseline_path) {
		Some(expected) => {
			if actual == expected {
				Ok(())
			} else if update_baselines {
				io_result_to_result(write_file(&full_baseline_path, &actual))
			} else {
				Err(TestFailure::UnexpectedOutput { actual, expected })
			}
		}
		None => {
			// This baseline didn't exist before.
			if update_baselines {
				io_result_to_result(write_file_and_ensure_directory(&full_baseline_path, &actual))
			} else {
				Err(TestFailure::NoSuchBaseline(full_baseline_path))
			}
		}
	}
}

fn io_result_to_result<T>(io_result: Result<T, IoError>) -> Result<T, TestFailure> {
	io_result.map_err(|e| TestFailure::IoError(e))
}

fn test_with_no_diagnostics(
	test_path: &Path,
	baselines_directory: &Path,
	program: &CompiledProgram,
	index_module: &Module,
	expected_baselines: &mut MutDict<Path, Arr<u8>>,
	update_baselines: bool,
) -> TestResult<()> {
	unused!(test_path, baselines_directory, program, index_module, expected_baselines, update_baselines);
	unimplemented!()
}



fn any_diagnostics(module: &Module) -> bool {
	module.diagnostics.any() || module.imports.some(|m| any_diagnostics(m))
}


/*

pub trait DocumentProvider {
	fn root_name(&self) -> Sym;
	/*
	None for document not found.
	Result::Err for any other error.
	*/
	fn get_document(&self, path: &Path) -> Result<Option<DocumentInfo>>;
}
*/
