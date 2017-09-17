use util::arena::Arena;
use util::dict::MutSet;
use util::late::Late;
use util::up::Up;

use model::expr::Expr;
use model::method::{MethodOrImpl, Parameter};
use model::module::Module;
use model::program::CompiledProgram;

use super::super::builtins::get_builtin;
use super::super::emitted_model::{Code, EmittedProgram, MethodMaps};

use super::emit_error::EmitResult;
use super::emit_expr::emit_method;

pub fn emit_program<'model : 'emit, 'emit>(
	program: &CompiledProgram<'model>,
	arena: &'emit Arena,
) -> EmitResult<'model, EmittedProgram<'model, 'emit>> {
	// Emit all dependencies first.
	let mut emitter =
		Emitter { arena, emitted_modules: MutSet::new(), methods: MethodMaps::new() };
	for b in *program.builtins.all_successes {
		emitter.emit_module(b)?
	}
	emitter.emit_module(program.root.assert_success())?;
	Ok(EmittedProgram { methods: emitter.methods })
}

struct Emitter<'model: 'emit, 'emit> {
	arena: &'emit Arena,
	emitted_modules: MutSet<Up<'model, Module<'model>>>,
	methods: MethodMaps<'model, 'emit>,
}
impl<'model, 'emit> Emitter<'model, 'emit> {
	fn is_module_already_emitted(&self, module: &Module<'model>) -> bool {
		self.emitted_modules.has(Up(module))
	}

	fn emit_module(&mut self, module: &Module<'model>) -> EmitResult<'model, ()> {
		// Emit dependencies first, so we'll be able to refer to them.
		for import in module.imports {
			if !self.is_module_already_emitted(import) {
				self.emit_module(import)?
			}
		}
		self.emit_single_module(module)
	}

	fn emit_single_module(&mut self, module: &Module<'model>) -> EmitResult<'model, ()> {
		let class = &module.class;

		// First make sure all methods in this module exist, then fill them in.
		for an_impl in class.all_impls() {
			let code = self.arena <- self.get_code_start(module, MethodOrImpl::Impl(Up(an_impl)), &*an_impl.body)?;
			self.methods.impls.add(Up(an_impl)) <- code;
		}

		for method in *class.methods {
			let code = self.arena <- self.get_code_start(module, MethodOrImpl::Method(Up(method)), &*method.body)?;
			self.methods.methods.add(Up(method)) <- code;
		}

		// Now fill them in.
		for an_impl in class.all_impls() {
			let implemented = &an_impl.implemented;
			if let Some(ref body) = *an_impl.body {
				self.fill_code(self.methods.get_impl(Up(an_impl)), implemented.parameters(), body, self.arena)
			}
		}
		for method in *class.methods {
			if let Some(ref body) = *method.body {
				self.fill_code(self.methods.get_method(Up(method)), method.parameters(), body, self.arena)
			}
		}

		Ok(())
	}

	fn get_code_start(
		&self,
		module: &Module<'model>,
		method: MethodOrImpl<'model>,
		body: &Option<Expr<'model>>,
	) -> EmitResult<'model, Code<'model, 'emit>> {
		Ok(match *body {
			Some(_) => Code::Instructions(Late::new()),
			None => Code::Builtin(get_builtin(module, method)?),
		})
	}

	fn fill_code(&self, code: &Code<'model, 'emit>, parameters: &'model [Parameter<'model>], body: &'model Expr<'model>, arena: &'emit Arena) {
		match *code {
			Code::Builtin(_) => unreachable!(),
			Code::Instructions(ref i) => {
				i <- emit_method(parameters, body, arena, &self.methods);
			}
		}
	}
}
