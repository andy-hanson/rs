use util::arena::{Arena, DirectBuilder};
use util::arith::{to_u8, u8_add, u8_add_mut, u8_sub, u8_sub_mut};
use util::loc::Loc;
use util::up::Up;

use model::expr::{Expr, ExprData, LiteralValue, Local, Pattern};
use model::method::{InstMethod, MethodOrImpl, MethodOrImplOrAbstract, Parameter};

use super::super::emitted_model::{Code, Instruction, Instructions, CalledInstructions, CalledBuiltin, MethodMaps};

pub fn emit_method<'model, 'emit>(
	parameters: &[Parameter<'model>],
	body: &'model Expr<'model>,
	arena: &'emit Arena,
	methods: &MethodMaps<'model, 'emit>,
) -> Instructions<'model, 'emit> {
	let n_parameters = to_u8(parameters.len());
	let mut emitter = ExprEmitter {
		arena,
		methods,
		w: InstructionWriter::new(arena),
		n_parameters,
		locals: Vec::new(),
		stack_depth: n_parameters,
	};
	emitter.emit_expr(body);
	emitter.w.finish()
}

struct InstructionWriter<'model: 'emit, 'emit> {
	instructions: DirectBuilder<'emit, Instruction<'model, 'emit>>,
}
impl<'model, 'emit> InstructionWriter<'model, 'emit> {
	fn new(arena: &'emit Arena) -> Self {
		InstructionWriter { instructions: arena.direct_builder() }
	}

	fn write(&mut self, loc: Loc, instruction: Instruction<'model, 'emit>) {
		unused!(loc);
		&mut self.instructions <- instruction;
	}

	fn finish(self) -> Instructions<'model, 'emit> {
		Instructions(self.instructions.finish())
	}
}

struct ExprEmitter<'model: 'emit, 'emit : 'maps, 'maps> {
	arena: &'emit Arena,
	methods: &'maps MethodMaps<'model, 'emit>,
	w: InstructionWriter<'model, 'emit>,
	// Number of parameters the current function has.
	n_parameters: u8,
	// Stack of all current locals.
	locals: Vec<Up<'model, Local<'model>>>,
	// Current stack depth relative to the start of this function (first parameter is 0).
	// This has a minimum of # parameters + # locals, but may be greater if
	// there are currently temporary values on the stack.
	stack_depth: u8,
}
impl<'model, 'emit, 'maps> ExprEmitter<'model, 'emit, 'maps> {
	fn write(&mut self, loc: Loc, instruction: Instruction<'model, 'emit>) {
		self.w.write(loc, instruction)
	}

	fn fetch(&mut self, loc: Loc, from_stack_depth: u8) {
		self.pushes(1);
		let depth_above_current = u8_sub(self.stack_depth, from_stack_depth);
		self.write(loc, Instruction::Fetch(depth_above_current))
	}

	fn pushes(&mut self, amount: u8) {
		u8_add_mut(&mut self.stack_depth, amount)
	}

	fn pops(&mut self, amount: u8) {
		u8_sub_mut(&mut self.stack_depth, amount)
	}

	fn emit_expr(&mut self, expr: &'model Expr<'model>) {
		let &Expr(loc, ref data) = expr;
		match *data {
			ExprData::Bogus | ExprData::BogusCast(_, _) =>
				// Should not reach here.
				unimplemented!(),
			ExprData::AccessParameter(param, _) =>
				self.fetch(loc, param.index),
			ExprData::AccessLocal(local) => {
				// Get the index of the local
				let index = self.locals.iter().position(|l| l.ptr_eq(local)).unwrap();
				let local_depth = u8_add(self.n_parameters, to_u8(index));
				self.fetch(loc, local_depth)
			}
			ExprData::Let(ref pattern, value, then) => {
				self.emit_expr(value);
				let n_pushed = match *pattern {
					Pattern::Ignore => unimplemented!(),
					Pattern::Single(local) => {
						self.locals.push(Up(local));
						1
					},
					Pattern::Destruct(_, _) => unimplemented!(),
				};

				self.emit_expr(then);

				self.write(loc, Instruction::UnLet(n_pushed));
				for _ in 0..n_pushed {
					self.locals.pop();
				}
				self.stack_depth -= n_pushed
			}
			ExprData::Seq(first, then) => {
				self.emit_expr(first);
				self.write(loc, Instruction::PopVoid);
				self.pops(1);
				self.emit_expr(then)
			}
			ExprData::Literal(ref value) => {
				self.pushes(1);
				self.write(loc, match *value {
					LiteralValue::Nat(n) => Instruction::LiteralNat(n),
					LiteralValue::Int(i) => Instruction::LiteralInt(i),
					LiteralValue::Float(f) => Instruction::LiteralFloat(f),
					LiteralValue::String(s) => {
						unused!(s);
						unimplemented!()//Instruction::LiteralString(Rc::clone(s)),
					}
				})
			},
			ExprData::IfElse { .. } => unimplemented!(),
			ExprData::WhenTest(_, _, _) => unimplemented!(),
			ExprData::Try { .. } => unimplemented!(),
			ExprData::For { .. } => unimplemented!(),
			ExprData::StaticMethodCall(method, args, _) => {
				for arg in args {
					self.emit_expr(arg);
				}
				let insn = self.call_instruction(method);
				self.write(loc, insn);
				self.pops(method.0.arity());
				self.pushes(1);
			},
			ExprData::InstanceMethodCall(_, _, _, _) => unimplemented!(),
			ExprData::MyInstanceMethodCall(_, _, _) => unimplemented!(),
			ExprData::New(_, _) => unimplemented!(),
			ExprData::ArrayLiteral { .. } => unimplemented!(),
			ExprData::GetMySlot(_, _) => unimplemented!(),
			ExprData::GetSlot(_, _, _) => unimplemented!(),
			ExprData::SetSlot(_, _) => unimplemented!(),
			ExprData::SelfExpr(_) => unimplemented!(),
			ExprData::Assert(_) => unimplemented!(),
			ExprData::Recur(_, _) => unimplemented!(),
		}
	}

	fn call_instruction(&self, inst_method: &'model InstMethod<'model>) -> Instruction<'model, 'emit> {
		if !inst_method.1.is_empty() {
			// Type arguments present
			unimplemented!()
		}

		match inst_method.0 {
			MethodOrImplOrAbstract::Method(m) =>
				self.call_code(MethodOrImpl::Method(m), self.methods.get_method(m)),
			MethodOrImplOrAbstract::Impl(i) =>
				self.call_code(MethodOrImpl::Impl(i), self.methods.get_impl(i)),
			MethodOrImplOrAbstract::Abstract(_) => {
				unimplemented!()
			}
		}
	}

	fn call_code(&self, m: MethodOrImpl<'model>, code: &'emit Code<'model, 'emit>) -> Instruction<'model, 'emit> {
		match *code {
			Code::Instructions(ref i) => Instruction::CallInstructions(CalledInstructions(m, Up(i))),
			Code::Builtin(b) => Instruction::CallBuiltin(CalledBuiltin(m, b)),
		}
	}
}
