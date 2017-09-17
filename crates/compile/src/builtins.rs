use util::arena::{Arena, ExactLenBuilder, MaxLenBuilder};
use util::file_utils::{ReadFileOptions, read_file};
use util::iter::KnownLen;
use util::late::Late;
use util::list::List;
use util::output_shower::OutputShower;
use util::path::Path;
use util::show::Shower;
use util::sym::Sym;
use util::up::Up;

use ast::Module as ModuleAst;
use parse::{parse, ParseDiagnostic};
use model::builtins::BuiltinsOwn;
use model::class::ClassDeclaration;
use model::diag::{Diag, Diagnostic};
use model::diag::show_diagnostics;
use model::module::{FailModule, Module, ModuleOrFail, ModuleSourceEnum};
use model::ty::{InstClass, Ty};

use super::check::check_module;

pub fn get_builtins<'model>(arena: &'model Arena) -> &'model BuiltinsOwn {
	let own = arena <- BuiltinsOwn {
		all: Late::new(),
		all_successes: Late::new(),
		void: Late::new(),
		bool: Late::new(),
		nat: Late::new(),
		int: Late::new(),
		float: Late::new(),
	};
	const LEN: usize = 5;
	let mut ctx = GetBuiltinsCtx { own, all: arena.exact_len_builder(LEN), all_successes: arena.max_len_builder(LEN), arena };

	&own.void <- ctx.add_builtin(b"Void", b"builtins/Void.nz"); //TODO: don't duplicate `builtins/` every time
	&own.bool <- ctx.add_builtin(b"Bool", b"builtins/Bool.nz");
	&own.nat <- ctx.add_builtin(b"Nat", b"builtins/Nat.nz");
	&own.int <- ctx.add_builtin(b"Int", b"builtins/Int.nz");
	&own.float <- ctx.add_builtin(b"Float", b"builtins/Float.nz");
	//TODO: string

	&own.all <- ctx.all.finish();
	own.all_successes.initialize_or_overwrite(ctx.all_successes.finish());
	own
}

struct GetBuiltinsCtx<'model> {
	own: &'model BuiltinsOwn<'model>,
	all: ExactLenBuilder<'model, ModuleOrFail<'model>>,
	all_successes: MaxLenBuilder<'model, Up<'model, Module<'model>>>,
	arena: &'model Arena,
}
impl<'model> GetBuiltinsCtx<'model> {
	fn add_builtin(&mut self, name_str: &'static [u8], file_name: &'static [u8]) -> Ty<'model> {
		let name = Sym::from_slice(name_str);
		let text = load_builtin_file(file_name, self.arena);
		let source = ModuleSourceEnum::Builtin { name, text };
		let ast_arena = Arena::new();
		let (ty, module_or_fail) = match parse(&ast_arena, text) {
			Ok(ModuleAst { imports, class }) => {
				let module = self.arena <- Module {
					source,
					imports: &[],
					class: Late::new(),
					diagnostics: Late::new(),
				};
				assert!(imports.is_empty());
				self.own.all_successes.initialize_or_overwrite(self.all_successes.slice_so_far());
				check_module(module, self.own, class, name, self.arena);
				&mut self.all_successes <- Up(module);
				(primitive_ty(&module.class), ModuleOrFail::Module(module))
			}
			Err(ParseDiagnostic(loc, parse_diag)) => {
				let diag = Diagnostic { loc, diag: Diag::ParseError(parse_diag) };
				let mf = ModuleOrFail::Fail(
					self.arena <- FailModule { source, imports: &[], diagnostics: List::single(diag, self.arena) },
				);
				let mut shower = OutputShower::stderr();
				show_diagnostics(mf, &mut shower).unwrap();
				shower.nl().unwrap();
				panic!("Bai")
				//(Ty::Bogus, ???)
				//TODO: just return it...
			}
		};
		&mut self.all <- module_or_fail;
		ty
	}
}

fn load_builtin_file<'model>(path_slice: &[u8], arena: &'model Arena) -> &'model [u8] {
	read_file(Path::of_slice(path_slice), ReadFileOptions::Trailing0, arena).unwrap().unwrap_or_else(|| {
		let str = String::from_utf8(path_slice.to_owned()).unwrap();
		panic!("Can't load builtin from {}", str)
	})
}

fn primitive_ty<'model>(class: &'model ClassDeclaration<'model>) -> Ty<'model> {
	Ty::pure_ty(InstClass { class: Up(class), ty_args: &[] })
}
