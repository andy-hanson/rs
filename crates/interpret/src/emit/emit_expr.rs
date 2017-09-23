use util::arena::{Arena, DirectBuilder};
use util::arith::{u8_add, u8_add_mut, u8_sub, u8_sub_mut, usize_to_u8};
use util::loc::Loc;
use util::up::Up;

use model::class::SlotDeclaration;
use model::expr::{Expr, ExprData, GetSlotData, InstanceMethodCallData, LetData, Local,
                  MyInstanceMethodCallData, Pattern, SeqData, StaticMethodCallData};
use model::method::{InstMethod, MethodOrImpl, MethodOrImplOrAbstract, Parameter};

use value::ValueCtx;

use super::super::emitted_model::{CalledBuiltin, CalledInstructions, Code, Instruction, Instructions,
                                  LocAndInstruction, MethodMaps};

pub fn emit_method<'model, 'value, 'emit>(
	value_ctx: &ValueCtx<'model, 'value>,
	has_self: bool,
	parameters: &[Parameter<'model>],
	body: &'model Expr<'model>,
	arena: &'emit Arena,
	methods: &MethodMaps<'model, 'emit>,
) -> Instructions<'model, 'emit> {
	let n_parameters = usize_to_u8(parameters.len());
	let arity = if has_self {
		n_parameters + 1
	} else {
		n_parameters
	};
	let mut emitter = ExprEmitter {
		value_ctx,
		arena,
		methods,
		w: InstructionWriter::new(arena),
		has_self,
		n_parameters,
		locals: Vec::new(),
		stack_depth: arity,
	};
	emitter.emit_expr(body);
	assert_eq!(emitter.stack_depth, arity + 1);
	emitter.write(body.loc, Instruction::Return);
	emitter.w.finish()
}

struct InstructionWriter<'model: 'emit, 'emit> {
	instructions: DirectBuilder<'emit, LocAndInstruction<'model, 'emit>>,
}
impl<'model, 'emit> InstructionWriter<'model, 'emit> {
	fn new(arena: &'emit Arena) -> Self {
		InstructionWriter { instructions: arena.direct_builder() }
	}

	fn write(&mut self, loc: Loc, instruction: Instruction<'model, 'emit>) {
		&mut self.instructions <- LocAndInstruction(loc, instruction);
	}

	fn finish(self) -> Instructions<'model, 'emit> {
		Instructions(self.instructions.finish())
	}
}

struct ExprEmitter<'value_ctx, 'model: 'value_ctx + 'value + 'emit, 'value: 'value_ctx, 'emit: 'maps, 'maps> {
	value_ctx: &'value_ctx ValueCtx<'model, 'value>,
	arena: &'emit Arena,
	methods: &'maps MethodMaps<'model, 'emit>,
	w: InstructionWriter<'model, 'emit>,
	has_self: bool,
	// Number of parameters the current function has, not including 'self'
	n_parameters: u8,
	// Stack of all current locals.
	locals: Vec<Up<'model, Local<'model>>>,
	// Current stack depth relative to the start of this function (first parameter is 0).
	// This has a minimum of # parameters + # locals, but may be greater if
	// there are currently temporary values on the stack.
	stack_depth: u8,
}
impl<'ctx, 'model, 'value, 'emit, 'maps> ExprEmitter<'ctx, 'model, 'value, 'emit, 'maps> {
	fn write(&mut self, loc: Loc, instruction: Instruction<'model, 'emit>) {
		self.w.write(loc, instruction)
	}

	// Takes an absolute stack depth; instruction uses depth relative to current stack top.
	fn fetch_at_depth(&mut self, loc: Loc, fetch_from_stack_depth: u8) {
		let by = u8_sub(self.stack_depth, fetch_from_stack_depth);
		self.write(loc, Instruction::Fetch(by));
		self.pushes(1);
	}

	fn fetch_self(&mut self, loc: Loc) {
		self.fetch_at_depth(loc, 0);
	}

	fn pushes(&mut self, amount: u8) {
		u8_add_mut(&mut self.stack_depth, amount)
	}

	fn pops(&mut self, amount: u8) {
		u8_sub_mut(&mut self.stack_depth, amount)
	}

	fn n_parameters_and_self(&self) -> u8 {
		if self.has_self {
			self.n_parameters + 1
		} else {
			self.n_parameters
		}
	}

	fn emit_expr(&mut self, expr: &'model Expr<'model>) {
		let &Expr { loc, ref ty, ref data } = expr;
		match *data {
			ExprData::Bogus =>
				// Should not reach here.
				unimplemented!(),
			ExprData::AccessParameter(param) => {
				let index = if self.has_self { param.index + 1 } else { param.index };
				self.fetch_at_depth(loc, index)
			}
			ExprData::AccessLocal(local) => {
				// Get the index of the local
				let index = self.locals.iter().position(|l| l.ptr_eq(local)).unwrap();
				let local_depth = u8_add(self.n_parameters_and_self(), usize_to_u8(index));
				self.fetch_at_depth(loc, local_depth)
			}
			ExprData::Let(&LetData { ref pattern, ref value, ref then }) => {
				self.emit_expr(value);
				let n_pushed = match **pattern {
					Pattern::Ignore => unimplemented!(),
					Pattern::Single(ref local) => {
						self.locals.push(Up(local));
						1
					},
					Pattern::Destruct(_, _) => unimplemented!(),
				};

				self.emit_expr(&**then);

				self.write(loc, Instruction::UnLet(n_pushed));
				for _ in 0..n_pushed {
					self.locals.pop();
				}
				self.stack_depth -= n_pushed
			}
			ExprData::Seq(&SeqData(ref first, ref then)) => {
				self.emit_expr(first);
				self.write(loc, Instruction::PopVoid);
				self.pops(1);
				self.emit_expr(then)
			}
			ExprData::LiteralNat(n) => {
				self.pushes(1);
				self.write(loc, Instruction::LiteralNat(n))
			}
			ExprData::LiteralInt(i) => {
				self.pushes(1);
				self.write(loc, Instruction::LiteralInt(i))
			}
			ExprData::LiteralFloat(f) => {
				self.pushes(1);
				self.write(loc, Instruction::LiteralFloat(f));
			}
			ExprData::LiteralString(s) => {
				self.pushes(1);
				self.write(loc, Instruction::LiteralString(s));
			}
			ExprData::IfElse { .. } => unimplemented!(),
			ExprData::WhenTest(_) => unimplemented!(),
			ExprData::Try(_) => unimplemented!(),
			ExprData::For(_) => unimplemented!(),
			ExprData::StaticMethodCall(&StaticMethodCallData { ref method, args }) => {
				for arg in args {
					self.emit_expr(arg);
				}
				let insn = self.call_instruction(method);
				self.write(loc, insn);
				self.pops(method.method_decl.full_arity());
				self.pushes(1);
			},
			ExprData::InstanceMethodCall(&InstanceMethodCallData { ref target, ref method, args }) => {
				self.emit_expr(target);
				for arg in args {
					self.emit_expr(arg);
				}
				let insn = self.call_instruction(method);
				self.write(loc, insn);
				self.pops(method.method_decl.full_arity());
				self.pushes(1);
			},
			ExprData::MyInstanceMethodCall(&MyInstanceMethodCallData { ref method, args }) => {
				self.fetch_self(loc);
				//TODO:DUPLICATE CODE
				for arg in args {
					self.emit_expr(arg);
				}
				let insn = self.call_instruction(method);
				self.write(loc, insn);
				self.pops(method.method_decl.full_arity());
				self.pushes(1);
			},
			ExprData::New(args) => {
				for arg in args {
					self.emit_expr(arg);
				}
				self.write(loc, Instruction::NewSlots(ty, args.len()));
				self.pops(usize_to_u8(args.len()));
				self.pushes(1);
			}
			ExprData::ArrayLiteral(_) => unimplemented!(),
			ExprData::GetMySlot(slot) => {
				self.fetch_self(loc);
				let insn = self.get_slot_instruction(slot);
				self.write(loc, insn);
			},
			ExprData::GetSlot(&GetSlotData { ref target, slot }) => {
				self.emit_expr(target);
				let insn = self.get_slot_instruction(slot);
				self.write(loc, insn);
				// Net 0 stack effect -- pops an object, pushes the value from the slot.
			},
			ExprData::SetSlot(_) => unimplemented!(),
			ExprData::SelfExpr => self.fetch_self(loc),
			ExprData::Assert(expr) => {
				self.emit_expr(expr);
				// Pops the boolean and pushes void -- 0 stack effect.
				self.write(loc, Instruction::Assert);
			},
			ExprData::Recur(_) => unimplemented!(),
		}
	}

	fn get_slot_instruction(&self, slot: Up<'model, SlotDeclaration<'model>>) -> Instruction<'model, 'emit> {
		let offset = self.value_ctx.get_slot_offset(slot);
		Instruction::GetSlot(offset)
	}

	fn call_instruction(&self, inst_method: &'model InstMethod<'model>) -> Instruction<'model, 'emit> {
		// Currently, all values are exactly one Value in size.
		// So we can compile a method body only once and it will work for all possible types.
		// if !inst_method.ty_args.is_empty() {}

		match inst_method.method_decl {
			MethodOrImplOrAbstract::Method(m) =>
				self.call_code(MethodOrImpl::Method(m), self.methods.get_method(m)),
			MethodOrImplOrAbstract::Impl(i) =>
				self.call_code(MethodOrImpl::Impl(i), self.methods.get_impl(i)),
			MethodOrImplOrAbstract::Abstract(_) => unimplemented!(),
		}
	}

	fn call_code(
		&self,
		m: MethodOrImpl<'model>,
		code: &'emit Code<'model, 'emit>,
	) -> Instruction<'model, 'emit> {
		match *code {
			Code::Instructions(ref i) => Instruction::CallInstructions(CalledInstructions(m, Up(i))),
			Code::Builtin(b) => Instruction::CallBuiltin(CalledBuiltin(m, b)),
		}
	}
}
