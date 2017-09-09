use util::arena::{Arena, Up};
use util::dict::{MutDict, MutSet};
use util::sym::Sym;

use super::super::model::expr::Expr;
use super::super::model::method::{Impl, MethodOrImpl, MethodWithBody, Parameter};
use super::super::model::module::Module;

use super::builtins::get_builtin;
use super::emitted_model::{Code, CodeData, EmittedProgram};

mod emit_expr;
use self::emit_expr::emit_method;

pub fn emit_program<'model, 'emit>(root_module: &'model Module<'model>, arena: &'emit Arena) -> EmittedProgram<'model, 'emit> {
	// Emit all dependencies first.
	let mut emitter =
		Emitter { arena, emitted_modules: MutSet::new(), methods: MutDict::new(), impls: MutDict::new() };
	emitter.emit_module(root_module);
	EmittedProgram { methods: emitter.methods.freeze(), impls: emitter.impls.freeze() }
}

struct Emitter<'model : 'emit, 'emit> {
	arena: &'emit Arena,
	emitted_modules: MutSet<Up<'model, Module<'model>>>,
	methods: MutDict<Up<'model, MethodWithBody<'model>>, Code<'model, 'emit>>,
	impls: MutDict<Up<'model, Impl<'model>>, Code<'model, 'emit>>,
}
impl<'model, 'emit> Emitter<'model, 'emit> {
	fn is_module_already_emitted(&self, module: &Module<'model>) -> bool {
		self.emitted_modules.has(&Up(module))
	}

	fn emit_module(&mut self, module: &Module<'model>) {
		// Emit dependencies first, so we'll be able to refer to them.
		for import in module.imports.iter() {
			if !self.is_module_already_emitted(import) {
				self.emit_module(import)
			}
		}
		self.emit_single_module(module)
	}

	fn emit_single_module(&mut self, module: &Module<'model>) {
		let class = &module.class;
		for zuper in class.supers.iter() {
			for an_impl in zuper.impls.iter() {
				let implemented = &an_impl.implemented;
				let code = self.get_code(module, implemented.name(), implemented.parameters(), *an_impl.body);
				self.impls
					.add(Up(an_impl), Code { source: MethodOrImpl::Impl(Up(an_impl)), code })
			}
		}

		for method in class.methods.iter() {
			let code = self.get_code(module, method.name(), method.parameters(), *method.body);
			self.methods.add(Up(method), Code { source: MethodOrImpl::Method(Up(method)), code })
		}
	}

	fn get_code(
		&self,
		module: &Module<'model>,
		implemented: Sym,
		parameters: &'model [Parameter<'model>],
		body: Option<&'model Expr<'model>>,
	) -> CodeData<'model, 'emit> {
		match body {
			Some(expr) => CodeData::Instructions(emit_method(parameters, expr, self.arena)),
			None => CodeData::Builtin(get_builtin(module, implemented)),
		}
	}
}
