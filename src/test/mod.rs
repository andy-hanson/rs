use std::cell::RefCell;

use util::arr::{Arr, SliceOps};
use util::dict::MutDict;
use util::loc::LineAndColumnLoc;
use util::path::Path;
use util::sym::Sym;

use super::host::document_info::DocumentInfo;
use super::host::document_provider::DocumentProvider;
use super::host::file_input::{FileInput, NativeFileInput, Result};

const TEST_DIR: &'static str = "tests";
lazy_static! {
	static ref CASES_ROOT_DIR: Path = Path::from_parts(arr![TEST_DIR, "cases",]);
	static ref BASELINES_ROOT_DIR: Path = Path::from_parts(arr![TEST_DIR, "builtins",]);
}

fn test_single(test_name: &'static str, update_baselines: bool) {
	//let test_data = run_compiler_test(Path::from_parts(arr![test_name]));
	//run_test(test-
	let test_directory = CASES_ROOT_DIR.child(Arr::copy_from_str(test_name));

	let document_provider = TestDocumentProvider::new(test_directory);
	unused!(document_provider, update_baselines);
	unimplemented!()
}


//mv
struct TestDocumentProvider {
	file_input: NativeFileInput,
	errors: RefCell<MutDict<Path, Arr<Error>>>,
}
impl TestDocumentProvider {
	fn new(test_directory: Path) -> TestDocumentProvider {
		TestDocumentProvider {
			file_input: NativeFileInput::new(test_directory),
			errors: RefCell::new(MutDict::new()),
		}
	}
}
impl DocumentProvider for TestDocumentProvider {
	fn root_name(&self) -> Sym {
		self.file_input.root_name()
	}

	fn get_document(&self, path: &Path) -> Result<Option<DocumentInfo>> {
		self.file_input.read(path).map(|op| {
			op.map(|content| {
				let (text_without_errors, errors) = parse_expected_errors(&content);
				if errors.any() {
					self.errors.borrow_mut().add(path.clone_path(), errors);
				}
				DocumentInfo::of(text_without_errors, /*version*/ 0)
			})
		})
	}
}

struct Error(LineAndColumnLoc, Arr<u8>);

//mv
fn parse_expected_errors(text: &[u8]) -> (Arr<u8>, Arr<Error>) {
	unused!(text);
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
