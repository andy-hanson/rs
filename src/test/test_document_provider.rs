use std::cell::RefCell;

use util::arith::usize_to_u32;
use util::arr::{Arr, ArrBuilder, SliceOps};
use util::dict::MutDict;
use util::loc::LineAndColumnLoc;
use util::path::Path;
use util::sym::Sym;

use super::super::host::document_info::DocumentInfo;
use super::super::host::document_provider::DocumentProvider;
use super::super::host::file_input::{FileInput, NativeFileInput, Result};

pub struct ExpectedDiagnostic(LineAndColumnLoc, Arr<u8>);

pub struct TestDocumentProvider {
	file_input: NativeFileInput,
	expected_diagnostics: RefCell<MutDict<Path, Arr<ExpectedDiagnostic>>>,
}
impl TestDocumentProvider {
	pub fn new(test_directory: Path) -> Self {
		TestDocumentProvider {
			file_input: NativeFileInput::new(test_directory),
			expected_diagnostics: RefCell::new(MutDict::new()),
		}
	}

	pub fn get_expected_diagnostics(self) -> MutDict<Path, Arr<ExpectedDiagnostic>> {
		self.expected_diagnostics.into_inner()
	}
}
impl DocumentProvider for TestDocumentProvider {
	fn root_name(&self) -> Sym {
		self.file_input.root_name()
	}

	fn get_document(&self, path: &Path) -> Result<Option<DocumentInfo>> {
		self.file_input.read(path).map(|op| {
			op.map(|content| {
				let (text_without_errors, errors) = parse_expected_errors(content);
				if errors.any() {
					self.expected_diagnostics
						.borrow_mut()
						.add(path.clone_path(), errors);
				}
				DocumentInfo::of(text_without_errors, /*version*/ 0)
			})
		})
	}
}

fn parse_expected_errors(code: Arr<u8>) -> (Arr<u8>, Arr<ExpectedDiagnostic>) {
	if !code.contains(&b'~') {
		return (code, Arr::empty())
	}

	let mut good_lines = ArrBuilder::<u8>::new();
	let mut expected_diagnostics = ArrBuilder::<ExpectedDiagnostic>::new();

	let mut good_line_number = 0;
	let mut last_good_start_index = 0;
	let mut last_good_line_end = 0; // To index *past* the '\n'.
	let mut last_good_line_indent = 0;
	let mut i = 0;

	let mut tab_indent = 0;
	let mut spaces_indent = 0;
	while i != code.len() {
		// This match only runs until we know whether we're on an error line or not.
		match code[i] {
			b'\t' => {
				if spaces_indent != 0 {
					// TODO: show a useful error message.
					// Test data should never have spaces before tabs.
					unimplemented!()
				}
				tab_indent += 1;
				i += 1
			}
			b' ' => {
				spaces_indent += 1;
				i += 1
			}
			b'~' => {
				if tab_indent != last_good_line_indent {
					// error: Error line should match tab indent of previous line.
					// (Other indent should be spaces.)
					unimplemented!()
				}

				let start = i;
				i += 1;
				while i != code.len() && code[i] == b'~' {
					i += 1
				}
				if i == code.len() {
					// Error: Should be followed by a `!` line.
					unimplemented!()
				}
				if code[i] != b'\n' {
					// error: If there is `~` on a line, that should be all there is.
					unimplemented!()
				}
				let diagnostic_width = usize_to_u32(i - start);
				i += 1; // Past the '\n'

				good_lines.add_slice(code.slice(last_good_start_index, last_good_line_end));

				let total_indent = tab_indent + spaces_indent;
				if good_line_number == 0 {
					// Error: File shouldn't begin with a `~` line
					unimplemented!()
				}
				// The error will apply to the previous line.
				let err_lc = LineAndColumnLoc::single_line(
					good_line_number - 1,
					total_indent,
					total_indent + diagnostic_width,
				);

				// Next line must contain error text.alloc
				if code[i] != b'!' {
					unimplemented!()
				}
				i += 1;
				if code[i] != b' ' {
					unimplemented!()
				}
				i += 1;

				let mut error_text = ArrBuilder::<u8>::new();
				while i != code.len() && code[i] != b'\n' {
					//TODO:PERF maybe just add_slice at the end
					error_text.add(code[i]);
					i += 1
				}
				if i != code.len() {
					i += 1
				}

				while i != code.len() && code[i] == b'!' {
					// Collect more lines of diagnostic text.
					i += 1;
					if code[i] != b' ' {
						// error: Must always begin with `! `
						unimplemented!()
					}
					error_text.add(b'\n');
					while i != code.len() && code[i] != b'\n' {
						error_text.add(code[i]);
						i += 1
					}
					if i != code.len() {
						i += 1
					}
				}

				expected_diagnostics.add(ExpectedDiagnostic(err_lc, error_text.finish()));

				last_good_start_index = i;
				last_good_line_end = i
			}
			_ => {
				while code[i] != b'\n' && i != code.len() {
					i += 1
				}
				if i != code.len() {
					i += 1;
					last_good_line_end = i;
					good_line_number += 1
				}
				last_good_line_indent = tab_indent
			}
		}
	}

	good_lines.add_slice(code.slice(last_good_start_index, code.len()));
	(good_lines.finish(), expected_diagnostics.finish())
}
