use util::up::Up;

use model::method::{MethodOrImpl, MethodWithBody};
use model::program::CompiledProgram;

use value::{Value, ValueCtx};

use super::emitted_model::{BuiltinCode, Code, EmittedProgram, Instruction, Instructions, CalledInstructions, CalledBuiltin};

pub fn run_method<'model, 'emit>(
	program: &CompiledProgram<'model>,
	method: Up<'model, MethodWithBody<'model>>,
	emitted: &EmittedProgram<'model, 'emit>,
) -> () {
	assert!(method.is_static);
	assert_eq!(0, method.arity());
	let mut ctx = ValueCtx::new(program.builtins);
	let emitted_method = emitted.methods.get_method(method);
	let res = exec(&mut ctx, emitted_method, MethodOrImpl::Method(method));
	res.assert_void(&ctx)
}

fn exec<'model : 'value, 'emit, 'value>(
	ctx: &mut ValueCtx<'model, 'value>,
	first_code: &Code<'model, 'emit>,
	mut cur_method: MethodOrImpl<'model>,
) -> Value<'model, 'value> {
	let mut stack = ctx.new_stack();
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
				stack.push(ctx.nat(n));
			}
			Instruction::LiteralInt(i) => {
				stack.push(ctx.int(i));
			}
			Instruction::LiteralFloat(f) => {
				stack.push(ctx.float(f));
			}
			Instruction::LiteralString(s) => {
				unused!(s);
				unimplemented!() //stack <- Value::String(Rc::clone(s)),
			}
			Instruction::Fetch(n) => stack.fetch(n),
			Instruction::UnLet(n) => stack.un_let(n),
			Instruction::PopVoid => stack.pop().assert_void(ctx),
			Instruction::CallInstructions(CalledInstructions(ref called_method, ref called_method_instructions)) => {
				return_stack.place_back() <- (cur_method, cur_instructions, instruction_index);
				cur_method = called_method.copy();
				cur_instructions = called_method_instructions;
				instruction_index = 0;
			}
			Instruction::CallBuiltin(CalledBuiltin(ref called_method, ref builtin)) => {
				unused!(called_method); //TODO: use this for error reporting
				match *builtin {
					BuiltinCode::Fn0(f) => {
						stack.push(f(ctx));
					}
					BuiltinCode::Fn1(f) => {
						let a = stack.pop();
						stack.push(f(ctx, a));
					}
					BuiltinCode::Fn2(f) => {
						let a = stack.pop();
						let b = stack.pop();
						stack.push(f(ctx, a, b));
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
