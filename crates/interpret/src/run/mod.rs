use util::arena::Arena;
use util::up::Up;

use model::method::{MethodOrImpl, MethodWithBody};
use model::program::CompiledProgram;

use value::ValueCtx;

use super::emitted_model::{BuiltinCode, Code, EmittedProgram, Instruction, Instructions, CalledInstructions, CalledBuiltin};

pub fn run_method<'model, 'emit>(
	program: &CompiledProgram<'model>,
	method: Up<'model, MethodWithBody<'model>>,
	emitted: &EmittedProgram<'model, 'emit>,
) -> () {
	assert!(method.is_static);
	assert_eq!(0, method.arity());
	let ctx_arena = Arena::new();
	let mut ctx = ValueCtx::new(program.builtins, &ctx_arena);
	let emitted_method = emitted.methods.get_method(method);
	exec(&mut ctx, emitted_method, MethodOrImpl::Method(method))
}

fn exec<'model : 'value, 'emit, 'value>(
	ctx: &'value mut ValueCtx<'model, 'value>,
	first_code: &Code<'model, 'emit>,
	mut cur_method: MethodOrImpl<'model>,
) {
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
				let nat = ctx.nat(n);
				ctx.push(nat);
			}
			Instruction::LiteralInt(i) => {
				let int = ctx.int(i);
				ctx.push(int);
			}
			Instruction::LiteralFloat(f) => {
				let float = ctx.float(f);
				ctx.push(float);
			}
			Instruction::LiteralString(s) => {
				unused!(s);
				unimplemented!()
			}
			Instruction::Fetch(n) => ctx.fetch(n),
			Instruction::UnLet(n) => ctx.un_let(n),
			Instruction::PopVoid => ctx.pop().assert_void(ctx),
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
						let res = f(ctx);
						ctx.push(res);
					}
					BuiltinCode::Fn1(f) => {
						let a = ctx.pop();
						let res = f(ctx, a);
						ctx.push(res);
					}
					BuiltinCode::Fn2(f) => {
						let a = ctx.pop();
						let b = ctx.pop();
						let res = f(ctx, a, b);
						ctx.push(res);
					}
				}
			}
			Instruction::Return => {
				//pop parameters
				ctx.un_let(cur_method.arity());
				match return_stack.pop() {
					Some((method, instructions, index)) => {
						cur_method = method;
						cur_instructions = instructions;
						instruction_index = index
					}
					None => break,
				}
			}
			Instruction::NewSlots(ty, n) => ctx.pop_slots_and_push_new_value(ty, n),
		}
	}

	assert_eq!(ctx.depth(), 1);
	let res = ctx.pop();
	res.assert_void(ctx)
}
