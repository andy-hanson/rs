use serde::Serialize;
use serde_json::to_string as to_json_string;

use std::io::stderr;

use util::arr::{Arr, SliceOps};
use util::dict::{Dict, MutDict};
use util::file_utils::{read_files_in_directory_recursive_if_exists, write_file,
                       write_file_and_ensure_directory, IoError};
use util::loc::LineAndColumnGetter;
use util::path::Path;
use util::string_maker::{Show, Shower, WriteShower};

mod test_document_provider;
use super::compiler::{compile, CompileResult, CompiledProgram, EXTENSION};
use super::model::diag::Diagnostic;
use super::model::module::{Module, OwnModuleOrFail, PtrModuleOrFail};

use self::test_document_provider::{ExpectedDiagnostic, TestDocumentProvider};

const TEST_DIR: &'static str = "tests";
lazy_static! {
	static ref CASES_ROOT_DIR: Path = Path::from_parts(arr![TEST_DIR, "cases",]);
	static ref BASELINES_ROOT_DIR: Path = Path::from_parts(arr![TEST_DIR, "builtins",]);
}

enum TestFailure {
	NoIndex(Path),
	IoError(IoError),
	ExtraBaselines(Arr<Path>),
	ExpectedDiagnostics(Dict<Path, Arr<ExpectedDiagnostic>>),
	UnexpectedDiagnostics(PtrModuleOrFail),
	NoSuchBaseline(Path),
	UnexpectedOutput { actual: Arr<u8>, expected: Arr<u8> },
}
impl Show for TestFailure {
	fn show<S: Shower>(&self, s: &mut S) {
		match *self {
			TestFailure::NoIndex(ref path) => {
				s.add(&"Could not find an index file at ").add(path);
			}
			TestFailure::IoError(_) => unimplemented!(),
			TestFailure::ExtraBaselines(_) => unimplemented!(),
			TestFailure::ExpectedDiagnostics(_) => unimplemented!(),
			TestFailure::UnexpectedDiagnostics(ref m) => show_unexpected_diagnostics(m, s),
			TestFailure::NoSuchBaseline(_) => unimplemented!(),
			TestFailure::UnexpectedOutput { .. } => unimplemented!(),
		}
	}
}

fn show_unexpected_diagnostics<S: Shower>(module: &PtrModuleOrFail, s: &mut S) {
	let diags = module.diagnostics();
	assert!(diags.any()); // Else we shouldn't have thrown the error

	let text = module.source().text();
	let lc = LineAndColumnGetter::new(text);
	for &Diagnostic(loc, ref data) in diags {
		let lc_loc = lc.line_and_column_at_loc(loc);
		s.add(&lc_loc).add(&" ").add(data);
	}
}

type TestResult<T> = ::std::result::Result<T, TestFailure>;

pub fn do_test_single(test_path: &Path, update_baselines: bool) -> i32 {
	match test_single(test_path, update_baselines) {
		Ok(()) => 0,
		Err(e) => {
			let mut shower = WriteShower::new(stderr());
			shower.add(&e).nl();
			1
		}
	}
}

fn test_single(test_path: &Path, update_baselines: bool) -> TestResult<()> {
	//let test_data = run_compiler_test(Path::from_parts(arr![test_name]));
	//run_test(test-
	let test_directory = Path::resolve_with_root(&CASES_ROOT_DIR, test_path);
	let baselines_directory = Path::resolve_with_root(&BASELINES_ROOT_DIR, test_path);

	let (program, root, mut expected_diagnostics, mut expected_baselines) = {
		let document_provider = TestDocumentProvider::new(test_directory);
		let (program, root) = {
			let compile_result = io_result_to_result(compile(Path::empty(), &document_provider, None))?;
			match compile_result {
				CompileResult::RootMissing =>
					return Err(TestFailure::NoIndex(document_provider.into_root_dir())),
				CompileResult::RootFound(program, root) => (program, root),
			}
		};
		let expected_diagnostics = document_provider.get_expected_diagnostics();
		let expected_baselines = io_result_to_result(
			read_files_in_directory_recursive_if_exists(&baselines_directory),
		)?.map_values(Arr::from_vec);
		(program, root, expected_diagnostics, expected_baselines)
	};

	check_builtin_diagnostics(&program)?;

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

	let res = if expected_baselines.any() {
		Err(TestFailure::ExtraBaselines(expected_baselines.into_keys()))
	} else {
		Ok(())
	};

	//TODO:KILL (and rely on implicit drop order)
	drop(expected_diagnostics);
	drop(program);

	res
}

const EXT_MODEL: &[u8] = b".model";

fn test_with_diagnostics(
	baselines_directory: &Path,
	program: &CompiledProgram,
	expected_baselines: &mut MutDict<Path, Arr<u8>>,
	expected_diagnostics_by_path: &mut MutDict<Path, Arr<ExpectedDiagnostic>>,
	update_baselines: bool,
) -> TestResult<()> {
	for module_or_fail in program.modules.values() {
		let source = module_or_fail.source().assert_normal();
		let module_full_path = source.full_path();
		let module_path = module_full_path.without_extension(EXTENSION);
		unused!(baselines_directory, expected_baselines, expected_diagnostics_by_path, module_path);

		if let OwnModuleOrFail::Module(ref module) = *module_or_fail {
			assert_baseline(
				baselines_directory,
				&module_path,
				EXT_MODEL,
				&module.class,
				expected_baselines,
				update_baselines,
			)?
		}

		let text = &source.document.text;
		let actual_diagnostics = module_or_fail.diagnostics();
		unused!(text, actual_diagnostics);
		unimplemented!()
	}
	unimplemented!()
}

//TODO:Should only have to do this once...
fn check_builtin_diagnostics(program: &CompiledProgram) -> TestResult<()> {
	let b = &program.builtins;
	for builtin_module in b.all.iter() {
		if builtin_module.diagnostics().any() {
			return Err(TestFailure::UnexpectedDiagnostics(builtin_module.to_ptr()))
		}
	}
	Ok(())
}

fn assert_baseline<T: Serialize>(
	test_directory: &Path,
	module_path: &Path,
	extension: &[u8],
	actual: &T,
	baselines: &mut MutDict<Path, Arr<u8>>,
	update_baselines: bool,
) -> TestResult<()> {
	assert_baseline_worker(
		test_directory,
		module_path,
		extension,
		Arr::from_string(to_json_string(actual).unwrap()),
		baselines,
		update_baselines,
	)
}

fn assert_baseline_worker(
	test_directory: &Path,
	module_path: &Path,
	extension: &[u8],
	actual: Arr<u8>,
	baselines: &mut MutDict<Path, Arr<u8>>,
	update_baselines: bool,
) -> TestResult<()> {
	let baseline_path = module_path.add_extension(extension);
	let full_baseline_path = Path::resolve_with_root(test_directory, &baseline_path);

	match baselines.try_extract(&baseline_path) {
		Some(expected) =>
			if actual == expected {
				Ok(())
			} else if update_baselines {
				io_result_to_result(write_file(&full_baseline_path, &actual))
			} else {
				Err(TestFailure::UnexpectedOutput { actual, expected })
			},
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
	io_result.map_err(TestFailure::IoError)
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
