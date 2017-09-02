use std::rc::Rc;

use util::arith::to_u8;
use util::ptr::{Own, Ptr};

use super::super::model::method::{MethodOrImpl, MethodWithBody};

use super::emitted_model::{BuiltinCode, Code, CodeData, EmittedProgram, Instruction, Instructions};
use super::value::Value;

mod data_stack;
use self::data_stack::DataStack;

pub fn run(method: &Ptr<MethodWithBody>, emitted: &EmittedProgram, arguments: Vec<Value>) -> Value {
	assert!(method.is_static);
	assert_eq!(to_u8(arguments.len()), method.arity());
	let emitted_method = emitted.methods.get(method).unwrap();
	exec(emitted_method, arguments)
}

fn exec(first_code: &Own<Code>, first_arguments: Vec<Value>) -> Value {
	let mut stack = DataStack::new(first_arguments);
	let mut return_stack = Vec::<(MethodOrImpl, Ptr<Instructions>, usize)>::new();
	let mut cur_method = first_code.source.copy();
	let mut cur_instructions = match first_code.code {
		CodeData::Instructions(ref i) => i.ptr(),
		_ => unreachable!(),
	};
	let mut instruction_index = 0;

	loop {
		let instruction = &cur_instructions.clone_ptr().0[instruction_index];
		instruction_index += 1;
		match *instruction {
			Instruction::LiteralVoid => stack.push(Value::Void),
			Instruction::LiteralBool(b) => stack.push(Value::Bool(b)),
			Instruction::LiteralNat(n) => stack.push(Value::Nat(n)),
			Instruction::LiteralInt(i) => stack.push(Value::Int(i)),
			Instruction::LiteralFloat(f) => stack.push(Value::Float(f)),
			Instruction::LiteralString(ref s) => stack.push(Value::String(Rc::clone(s))),
			Instruction::Fetch(n) => stack.fetch(n),
			Instruction::UnLet(n) => stack.un_let(n),
			Instruction::PopVoid => {
				let value = stack.pop();
				match value {
					Value::Void => {}
					_ => unreachable!(),
				}
			}
			Instruction::Call(ref called_method, ref called_method_instructions) => {
				return_stack.push((cur_method, cur_instructions.clone_ptr(), instruction_index));
				cur_method = called_method.copy();
				cur_instructions = called_method_instructions.clone_ptr();
				instruction_index = 0
			}
			Instruction::CallBuiltin(ref called_method, ref builtin) => {
				unused!(called_method); //TODO: use this for error reporting
				match *builtin {
					BuiltinCode::Fn0(f) => stack.push(f()),
					BuiltinCode::Fn1(f) => {
						let a = stack.pop();
						stack.push(f(a))
					}
					BuiltinCode::Fn2(f) => {
						let a = stack.pop();
						let b = stack.pop();
						stack.push(f(a, b))
					}
				}
			}
			Instruction::Return => {
				//pop parameters
				stack.un_let(cur_method.arity());
				match return_stack.pop() {
					Some((method, instructions, index)) => {
						cur_method = method;
						cur_instructions = instructions;
						instruction_index = index
					}
					None => break,
				}
			}
		}
	}

	assert_eq!(stack.depth(), 1);
	stack.pop()
}
