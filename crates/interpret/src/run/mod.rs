use std::marker::PhantomData;

use util::arith::to_u8;
use util::up::Up;

use model::method::{MethodOrImpl, MethodWithBody};

use super::emitted_model::{BuiltinCode, Code, EmittedProgram, Instruction, Instructions, CalledInstructions, CalledBuiltin};
use super::value::Value;

mod data_stack;
use self::data_stack::DataStack;

pub fn run_method<'model, 'emit>(
	method: Up<'model, MethodWithBody<'model>>,
	emitted: &EmittedProgram<'model, 'emit>,
	arguments: Vec<Value<'model>>,
) -> Value<'model> {
	assert!(method.is_static);
	assert_eq!(to_u8(arguments.len()), method.arity());
	let emitted_method = emitted.methods.get_method(method);
	exec(emitted_method, MethodOrImpl::Method(method), arguments)
}

fn exec<'model, 'emit>(
	first_code: &Code<'model, 'emit>,
	mut cur_method: MethodOrImpl<'model>,
	first_arguments: Vec<Value<'model>>,
) -> Value<'model> {
	let mut stack = DataStack::new(first_arguments);
	let mut return_stack = Vec::<(MethodOrImpl, &'emit Instructions, usize)>::new();
	let mut cur_instructions: &Instructions<'model, 'emit> = match *first_code {
		Code::Instructions(ref i) => i,
		_ => unreachable!(),
	};
	let mut instruction_index = 0;

	loop {
		let instruction = &cur_instructions.0[instruction_index];
		instruction_index += 1;
		match *instruction {
			Instruction::LiteralNat(n) => {
				&mut stack <- Value::Nat(n);
			}
			Instruction::LiteralInt(i) => {
				&mut stack <- Value::Int(i);
			}
			Instruction::LiteralFloat(f) => {
				&mut stack <- Value::Float(f);
			}
			Instruction::LiteralString(s) => {
				unused!(s);
				unimplemented!() //stack <- Value::String(Rc::clone(s)),
			}
			Instruction::Fetch(n) => stack.fetch(n),
			Instruction::UnLet(n) => stack.un_let(n),
			Instruction::PopVoid => {
				let value = stack.pop();
				match value {
					Value::Void => {}
					_ => unreachable!(),
				}
			}
			Instruction::CallInstructions(CalledInstructions(ref called_method, ref called_method_instructions)) => {
				return_stack.place_back() <- (cur_method, cur_instructions, instruction_index);
				cur_method = called_method.copy();
				cur_instructions = called_method_instructions;
				instruction_index = 0
			}
			Instruction::CallBuiltin(CalledBuiltin(ref called_method, ref builtin)) => {
				unused!(called_method); //TODO: use this for error reporting
				match *builtin {
					BuiltinCode::Fn0(f) => {
						&mut stack <- f(PhantomData);
					}
					BuiltinCode::Fn1(f) => {
						let a = stack.pop();
						&mut stack <- f(a);
					}
					BuiltinCode::Fn2(f) => {
						let a = stack.pop();
						let b = stack.pop();
						&mut stack <- f(a, b);
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