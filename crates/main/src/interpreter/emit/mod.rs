use util::dict::{MutDict, MutSet};
use util::ptr::{Own, Ptr};
use util::sym::Sym;

use super::super::model::expr::Expr;
use super::super::model::method::{Impl, MethodOrImpl, MethodWithBody, Parameter};
use super::super::model::module::Module;

use super::builtins::get_builtin;
use super::emitted_model::{Code, CodeData, EmittedProgram};

mod emit_expr;
use self::emit_expr::emit_method;

pub fn emit_program(root_module: &Module) -> EmittedProgram {
	// Emit all dependencies first.
	let mut emitter =
		Emitter { emitted_modules: MutSet::new(), methods: MutDict::new(), impls: MutDict::new() };
	emitter.emit_module(root_module);
	EmittedProgram { methods: emitter.methods.freeze(), impls: emitter.impls.freeze() }
}

struct Emitter {
	emitted_modules: MutSet<Ptr<Module>>,
	methods: MutDict<Ptr<MethodWithBody>, Own<Code>>,
	impls: MutDict<Ptr<Impl>, Own<Code>>,
}
impl Emitter {
	fn is_module_already_emitted(&self, module: &Ptr<Module>) -> bool {
		self.emitted_modules.has(module)
	}

	fn emit_module(&mut self, module: &Module) {
		// Emit dependencies first, so we'll be able to refer to them.
		for import in module.imports.iter() {
			if !self.is_module_already_emitted(import) {
				self.emit_module(import)
			}
		}
		self.emit_single_module(module)
	}

	fn emit_single_module(&mut self, module: &Module) {
		let class = &module.class;
		for zuper in class.supers.iter() {
			for an_impl in zuper.impls.iter() {
				let implemented = &an_impl.implemented;
				let code = self.get_code(module, implemented.name(), implemented.parameters(), &an_impl.body);
				self.impls
					.add(an_impl.ptr(), Own::new(Code { source: MethodOrImpl::Impl(an_impl.ptr()), code }))
			}
		}

		for method in class.methods.iter() {
			let code = self.get_code(module, method.name(), method.parameters(), &method.body);
			self.methods
				.add(method.ptr(), Own::new(Code { source: MethodOrImpl::Method(method.ptr()), code }))
		}
	}

	fn get_code(
		&self,
		module: &Module,
		implemented: Sym,
		parameters: &[Own<Parameter>],
		body: &Option<Expr>,
	) -> CodeData {
		match *body {
			Some(ref expr) => CodeData::Instructions(Own::new(emit_method(parameters, expr))),
			None => CodeData::Builtin(get_builtin(module, implemented)),
		}
	}
}
