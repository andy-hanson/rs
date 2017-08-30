use super::super::compiler::model::module::Module;

pub struct EmittedProgram {}

pub fn emit_program(root_module: &Module) -> EmittedProgram {
	// Emit all dependencies first.
	unused!(root_module);
	todo!()
}

struct Emitter {}
impl Emitter {
	fn emit_module(module: &Module) {
		unused!(module);
		todo!()
	}
}
