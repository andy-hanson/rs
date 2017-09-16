use util::arena::{Arena, NoDrop};
use util::arith::usize_to_u32;
use util::dict::MutDict;
use util::file_utils::IoError;
use util::loc::LineAndColumnLoc;
use util::path::Path;
use util::sym::Sym;

use host::document_provider::DocumentProvider;
use host::file_input::{FileInput, NativeFileInput};

use model::document_info::DocumentInfo;

pub struct ExpectedDiagnostic<'a>(pub LineAndColumnLoc, pub &'a [u8]);
impl<'a> NoDrop for ExpectedDiagnostic<'a> {}

pub struct TestDocumentProvider<'a> {
	file_input: NativeFileInput<'a>,
	expected_diagnostics: MutDict<Path<'a>, &'a [ExpectedDiagnostic<'a>]>,
}
impl<'a> TestDocumentProvider<'a> {
	pub fn into_root_dir(self) -> Path<'a> {
		self.file_input.root_dir
	}

	pub fn new(test_directory: Path<'a>) -> Self {
		TestDocumentProvider {
			file_input: NativeFileInput::new(test_directory),
			expected_diagnostics: MutDict::new(),
		}
	}

	pub fn get_expected_diagnostics(self) -> MutDict<Path<'a>, &'a [ExpectedDiagnostic<'a>]> {
		self.expected_diagnostics
	}
}
impl<'a> DocumentProvider<'a> for TestDocumentProvider<'a> {
	type Error = IoError;

	fn root_name(&self) -> Sym {
		self.file_input.root_name()
	}

	fn get_document(
		&mut self,
		path: Path,
		arena: &'a Arena,
	) -> Result<Option<DocumentInfo<'a>>, Self::Error> {
		//TODO:PERF parse while reading, avoid extra allocation
		self.file_input.read(path, arena).map(|op| {
			op.map(|content| {
				let (text_without_errors, errors) = parse_expected_errors(content, arena);
				if !errors.is_empty() {
					self.expected_diagnostics
						.add(path.clone_path_to_arena(arena)) <- errors;
				}
				DocumentInfo::of(text_without_errors, /*version*/ 0)
			})
		})
	}
}

fn parse_expected_errors<'a>(code: &'a [u8], arena: &'a Arena) -> (&'a [u8], &'a [ExpectedDiagnostic<'a>]) {
	let (good_lines_len, diagnostic_count) = count_diagnostics(code);
	if diagnostic_count == 0 {
		assert_eq!(good_lines_len, code.len());
		return (code, &[])
	}

	let mut good_lines = arena.exact_len_builder::<u8>(good_lines_len);
	let mut expected_diagnostics = arena.exact_len_builder::<ExpectedDiagnostic>(diagnostic_count);

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

				good_lines.add_slice(&code[last_good_start_index..last_good_line_end]);

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

				let mut error_text = arena.direct_builder::<u8>();
				while i != code.len() && code[i] != b'\n' {
					//TODO:PERF maybe just add_slice at the end
					&mut error_text <- code[i];
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
					&mut error_text <- b'\n';
					while i != code.len() && code[i] != b'\n' {
						&mut error_text <- code[i];
						i += 1
					}
					if i != code.len() {
						i += 1
					}
				}

				&mut expected_diagnostics <- ExpectedDiagnostic(err_lc, error_text.finish());

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

	good_lines.add_slice(&code[last_good_start_index..code.len()]);
	(good_lines.finish(), expected_diagnostics.finish())
}

fn count_diagnostics(text: &[u8]) -> (usize, usize) {
	let mut good_lines_len = 0;
	let mut diagnostics_count = 0;
	let mut iter = text.iter();
	'outer: while let Some(&ch) = iter.next() {
		if ch == b'~' {
			diagnostics_count += 1;
			// Eat any more '~'
			loop {
				match iter.next() {
					Some(&ch) =>
						if ch != b'~' {
							good_lines_len += 1;
							break
						},
					None => break 'outer,
				}
			}
		} else {
			good_lines_len += 1
		}
	}
	(good_lines_len, diagnostics_count)
}
