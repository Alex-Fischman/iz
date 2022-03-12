use crate::typer::*;

#[derive(Debug)]
pub enum IR {
	Push(i64),
	Add,
	Sub,
	Mul,
	Eql,
}

pub fn compile(ast: &TypedAST) -> Result<Vec<IR>, String> {
	let mut out = vec![];
	match ast {
		TypedAST::Token(token, t) => match t {
			Type::Data(s, xs) if s == "int" && xs.is_empty() => {
				out.push(IR::Push(token.string.parse().unwrap()))
			}
			Type::Data(s, xs) if s == "bool" && xs.is_empty() => match &*token.string {
				"true" => out.push(IR::Push(1)),
				"false" => out.push(IR::Push(0)),
				s => Err(format!("unknown bool value: {:?}", s))?,
			},
			data @ Type::Data(_, _) => Err(format!("unknown data type: {:?}", data))?,
			t @ Type::Func(_, _) => match &*token.string {
				"add" if t == &func(int(), func(int(), int())) => out.push(IR::Add),
				"neg" if t == &func(int(), int()) => out.extend([IR::Push(0), IR::Sub]),
				"sub" if t == &func(int(), func(int(), int())) => out.push(IR::Sub),
				"mul" if t == &func(int(), func(int(), int())) => out.push(IR::Mul),
				"eql" if t == &func(int(), func(int(), boolean())) => out.push(IR::Eql),
				"_if_" => todo!(),
				"_else_" => todo!(),
				s => Err(format!("unknown function {:?} with type {:?}", s, t))?,
			},
			Type::Var(_) => unreachable!(),
		},
		TypedAST::List(token, xs, _, _) => match &*token.string {
			"(" => out.extend(compile(&xs[0])?),
			"{" => {
				for x in xs {
					out.extend(compile(x)?);
				}
			}
			"[" => todo!(),
			s => Err(format!("unknown bracket: {:?}", s))?,
		},
		TypedAST::Call(f, x, _) => {
			out.extend(compile(x)?);
			out.extend(compile(f)?);
		}
	}
	Ok(out)
}

pub fn interpret(program: &[IR]) -> Vec<i64> {
	let mut stack: Vec<i64> = vec![];
	let mut i = 0;
	while i < program.len() {
		if let IR::Push(data) = program[i] {
			stack.push(data);
			i += 1;
			continue;
		}
		let a = stack.pop().unwrap();
		let b = stack.pop().unwrap();
		stack.push(match program[i] {
			IR::Push(_) => unreachable!(),
			IR::Add => a + b,
			IR::Sub => a - b,
			IR::Mul => a * b,
			IR::Eql => (a == b) as i64,
		});
		i += 1;
	}
	stack
}
