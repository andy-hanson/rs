use util::arr::{Arr, SliceOps};
use util::dict::{Dict, MutDict};
use util::path::Path;

mod test_document_provider;
use super::compiler::{compile, CompileResult, CompiledProgram, EXTENSION};
use super::host::file_input::Error as IoError;
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
}

fn test_single(test_path: &Path, update_baselines: bool) -> Result<(), TestFailure> {
	//let test_data = run_compiler_test(Path::from_parts(arr![test_name]));
	//run_test(test-
	let test_directory = Path::resolve_with_root(&CASES_ROOT_DIR, test_path);
	let baselines_directory = Path::resolve_with_root(&BASELINES_ROOT_DIR, test_path);

	let document_provider = TestDocumentProvider::new(test_directory);
	let (program, root) = match compile(Path::empty(), &document_provider, None) {
		Ok(r) =>
			match r {
				CompileResult::RootMissing => return Err(TestFailure::NoIndex),
				CompileResult::RootFound(program, root) => (program, root),
			},
		Err(e) => return Err(TestFailure::IoError(e)),
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
				)
			},
		PtrModuleOrFail::Fail(_) =>
			test_with_diagnostics(
				&baselines_directory,
				&program,
				&mut expected_baselines,
				&mut expected_diagnostics,
			),
	};

	unused!(update_baselines);
	if expected_baselines.any() {
		Err(TestFailure::ExtraBaselines(expected_baselines.into_keys()))
	} else {
		Ok(())
	}
}

fn test_with_diagnostics(
	baselines_directory: &Path,
	program: &CompiledProgram,
	expected_baselines: &mut MutDict<Path, Arr<u8>>,
	expected_diagnostics_by_path: &mut MutDict<Path, Arr<ExpectedDiagnostic>>,
) {
	for module_or_fail in program.modules.values() {
		let source = module_or_fail.source().unwrap();
		let module_full_path = source.full_path();
		let module_path = module_full_path.without_extension(&EXTENSION);
		unused!(baselines_directory, expected_baselines, expected_diagnostics_by_path, module_path);

		if let OwnModuleOrFail::Module(ref m) = *module_or_fail {
			//assert_baseline(baselinesDirectory, modulePath, ".model", module.klass.toDat(), expectedBaselines, updateBaselines);
			unused!(m);
			unimplemented!()
		}

		let text = &source.document.text;
		let actual_diagnostics = module_or_fail.diagnostics();
		unused!(text, actual_diagnostics);
		unimplemented!()

	}
}

fn test_with_no_diagnostics(
	test_path: &Path,
	baselines_directory: &Path,
	program: &CompiledProgram,
	index_module: &Module,
	expected_baselines: &mut MutDict<Path, Arr<u8>>,
) {
	unused!(test_path, baselines_directory, program, index_module, expected_baselines);
	unimplemented!()
}



fn any_diagnostics(module: &Module) -> bool {
	module.diagnostics.any() || module.imports.some(|m| any_diagnostics(m))
}

//mv
fn read_files_in_directory_recursive_if_exists(dir: &Path) -> MutDict<Path, Arr<u8>> {
	unused!(dir);
	unimplemented!()
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
