use util::arith::usub;
use util::loc::Loc;
use util::ptr::Ptr;
use util::vec::find_index;

use super::super::super::compiler::model::expr::{Expr, ExprData, LiteralValue, Local, Pattern};

use super::super::instruction::Instruction;
use super::super::value::Value;

pub trait InstructionWriter {
	fn write(&mut self, loc: Loc, i: Instruction) -> ();
}

struct ExprEmitter<'a, W : 'a + InstructionWriter> {
	w: &'a mut W,
	// Number of parameters the current function has.
	n_parameters: u32,
	// Stack of all current locals.
	locals: Vec<Ptr<Local>>,
	// Current stack depth relative to the start of this function (first parameter is 0).
	// This has a minimum of # parameters + # locals, but may be greater if
	// there are currently temporary values on the stack.
	stack_depth: u32,
}
impl<'a, W : InstructionWriter> ExprEmitter<'a, W> {
	fn write(&mut self, loc: Loc, instruction: Instruction) {
		self.w.write(loc, instruction)
	}

	fn fetch(&mut self, loc: Loc, by: u32) {
		self.pushes(1);
		let depth_above_current = usub(self.stack_depth, by);
		self.write(loc, Instruction::Fetch(depth_above_current))
	}

	fn pushes(&mut self, amount: u32) {
		self.stack_depth += amount
	}

	fn pops(&mut self, amount: u32) {
		self.stack_depth -= amount
	}

	fn emit_expr(&mut self, expr: &'a Expr) {
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
				let local_depth = self.n_parameters + index as u32;
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
				let v = match *value {
					LiteralValue::Pass => Value::Void,
					LiteralValue::Bool(b) => Value::Bool(b),
					LiteralValue::Nat(n) => Value::Nat(n),
					LiteralValue::Int(i) => Value::Int(i),
					LiteralValue::Float(f) => Value::Float(f),
					LiteralValue::String(ref s) => Value::String(s.clone()),
				};
				self.pushes(1);
				self.write(loc, Instruction::Literal(v))
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
