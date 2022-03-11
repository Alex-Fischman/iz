use crate::typer::Type;
use crate::typer::TypedAST;

type Data = u64;
const REGISTERS: usize = 4;

#[derive(Debug)]
pub enum IR {
	Set(Data, usize),
	Add(usize, usize, usize),
	Beq(usize, usize, usize),
	Push(usize),
	Pop(usize),
}

pub fn compile(ast: &TypedAST) -> Result<Vec<IR>, String> {
	let mut out = vec![];
	match ast {
		TypedAST::Token(token, t) => match t {
			Type::Data(s, xs) if s == "int" && xs.is_empty() => {
				out.push(IR::Set(token.string.parse().unwrap(), 0))
			}
			Type::Data(s, xs) if s == "bool" && xs.is_empty() => match &*token.string {
				"true" => out.push(IR::Set(1, 0)),
				"false" => out.push(IR::Set(0, 0)),
				s => Err(format!("unknown bool value: {:?}", s))?,
			},
			data @ Type::Data(_, _) => Err(format!("unknown data type: {:?}", data))?,
			Type::Func(_, _) => match &*token.string {
				"add" => out.extend([IR::Pop(0), IR::Pop(1), IR::Add(0, 1, 0)]),
				s => Err(format!("unknown function: {:?}", s))?,
			},
			Type::Var(_) => unreachable!(),
		},
		TypedAST::List(token, xs, _, _) => match &*token.string {
			"{" => {
				for x in xs {
					out.extend(compile(x)?);
				}
			}
			s => Err(format!("unknown bracket: {:?}", s))?,
		},
		TypedAST::Call(f, x, _) => {
			out.extend(compile(x)?);
			out.push(IR::Push(0));
			out.extend(compile(f)?);
		}
	}
	Ok(out)
}

pub fn interpret(program: &[IR]) -> ([Data; REGISTERS], Vec<Data>) {
	let mut registers = [0; REGISTERS];
	let mut stack = vec![];
	let mut i = 0;
	while i < program.len() {
		match program[i] {
			IR::Set(data, dest) => registers[dest] = data,
			IR::Add(a, b, dest) => registers[dest] = registers[a] + registers[b],
			IR::Beq(a, b, dest) => {
				if registers[a] == registers[b] {
					i += registers[dest] as usize;
				}
			}
			IR::Push(src) => stack.push(registers[src]),
			IR::Pop(dest) => registers[dest] = stack.pop().unwrap(),
		}
		i += 1;
	}
	(registers, stack)
}
