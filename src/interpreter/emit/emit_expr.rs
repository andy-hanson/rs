use util::arith::{to_u8, u8_add, u8_add_mut, u8_sub, u8_sub_mut};
use util::arr::{Arr, ArrBuilder};
use util::loc::Loc;
use util::ptr::{Own, Ptr};
use util::vec::find_index;

use super::super::super::compiler::model::expr::{Expr, ExprData, LiteralValue, Local, Pattern};
use super::super::super::compiler::model::method::Parameter;

use super::super::emitted_model::{Instruction, Instructions};

pub fn emit_method(parameters: &Arr<Own<Parameter>>, body: &Expr) -> Instructions {
	let n_parameters = to_u8(parameters.len());
	let mut emitter = ExprEmitter {
		w: InstructionWriter::new(),
		n_parameters,
		locals: Vec::new(),
		stack_depth: n_parameters,
	};
	emitter.emit_expr(body);
	emitter.w.finish()
}

struct InstructionWriter {
	instructions: ArrBuilder<Instruction>,
}
impl InstructionWriter {
	fn new() -> InstructionWriter {
		InstructionWriter { instructions: ArrBuilder::new() }
	}

	fn write(&mut self, loc: Loc, instruction: Instruction) {
		unused!(loc);
		self.instructions.add(instruction)
	}

	fn finish(self) -> Instructions {
		Instructions(self.instructions.finish())
	}
}

struct ExprEmitter {
	w: InstructionWriter,
	// Number of parameters the current function has.
	n_parameters: u8,
	// Stack of all current locals.
	locals: Vec<Ptr<Local>>,
	// Current stack depth relative to the start of this function (first parameter is 0).
	// This has a minimum of # parameters + # locals, but may be greater if
	// there are currently temporary values on the stack.
	stack_depth: u8,
}
impl ExprEmitter {
	fn write(&mut self, loc: Loc, instruction: Instruction) {
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

	fn emit_expr(&mut self, expr: &Expr) {
		let &Expr(loc, ref data) = expr;
		match *data {
			ExprData::Bogus | ExprData::BogusCast(_, _) =>
				// Should not reach here.
				todo!(),
			ExprData::AccessParameter(ref param, _) =>
				self.fetch(loc, param.index),
			ExprData::AccessLocal(ref local) => {
				// Get the index of the local
				let index = find_index(&self.locals, |l| l.ptr_equals(local)).unwrap();
				let local_depth = u8_add(self.n_parameters, to_u8(index));
				self.fetch(loc, local_depth)
			}
			ExprData::Let(ref pattern, ref value, ref then) => {
				self.emit_expr(value);
				let n_pushed = match *pattern {
					Pattern::Ignore => todo!(),
					Pattern::Single(ref local) => {
						self.locals.push(local.ptr());
						1
					},
					Pattern::Destruct(_, _) => todo!(),
				};

				self.emit_expr(then);

				self.write(loc, Instruction::UnLet(n_pushed));
				for _ in 0..n_pushed {
					self.locals.pop();
				}
				self.stack_depth -= n_pushed
			}
			ExprData::Seq(ref first, ref then) => {
				self.emit_expr(&first);
				self.write(loc, Instruction::PopVoid);
				self.pops(1);
				self.emit_expr(&then)
			}
			ExprData::Literal(ref value) => {
				self.pushes(1);
				self.write(loc, match *value {
					LiteralValue::Pass => Instruction::LiteralVoid,
					LiteralValue::Bool(b) => Instruction::LiteralBool(b),
					LiteralValue::Nat(n) => Instruction::LiteralNat(n),
					LiteralValue::Int(i) => Instruction::LiteralInt(i),
					LiteralValue::Float(f) => Instruction::LiteralFloat(f),
					LiteralValue::String(ref s) => Instruction::LiteralString(s.clone()),
				})
			},
			ExprData::IfElse { test: _, then: _, elze: _, ty: _ } => todo!(),
			ExprData::WhenTest(_, _, _) => todo!(),
			ExprData::Try { body: _, catch: _, finally: _, ty: _ } => todo!(),
			ExprData::For(_) => todo!(),
			ExprData::StaticMethodCall(_, _, _) => todo!(),
			ExprData::InstanceMethodCall(_, _, _, _) => todo!(),
			ExprData::MyInstanceMethodCall(_, _, _) => todo!(),
			ExprData::New(_, _) => todo!(),
			ExprData::ArrayLiteral { element_ty: _, elements: _ } => todo!(),
			ExprData::GetMySlot(_, _) => todo!(),
			ExprData::GetSlot(_, _, _) => todo!(),
			ExprData::SetSlot(_, _) => todo!(),
			ExprData::SelfExpr(_) => todo!(),
			ExprData::Assert(_) => todo!(),
			ExprData::Recur(_, _) => todo!(),
		}
	}
}
