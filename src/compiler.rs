use crate::typer::*;

#[derive(Clone, Debug)]
pub enum IR {
	Push(Frame),
	Add,
	Sub,
	Mul,
	Eql,
}

pub fn compile(ast: &TypedAST) -> Result<Vec<IR>, String> {
	ast.walk(
		&mut |token, t| {
			match t {
				Type::Data(s, xs) if s == "int" && xs.is_empty() => {
					Ok(vec![IR::Push(Int(token.string.parse().unwrap()))])
				}
				Type::Data(s, xs) if s == "bool" && xs.is_empty() => match &*token.string {
					"true" => Ok(vec![IR::Push(Int(1))]),
					"false" => Ok(vec![IR::Push(Int(0))]),
					s => Err(format!("unknown bool value: {:?}", s))?,
				},
				data @ Type::Data(_, _) => Err(format!("unknown data type: {:?}", data))?,
				t @ Type::Func(_, _) => match &*token.string {
					"add" if t == &func(int(), func(int(), int())) => Ok(vec![IR::Add]),
					"neg" if t == &func(int(), int()) => Ok(vec![IR::Push(Int(0)), IR::Sub]),
					"sub" if t == &func(int(), func(int(), int())) => Ok(vec![IR::Sub]),
					"mul" if t == &func(int(), func(int(), int())) => Ok(vec![IR::Mul]),
					"eql" if t == &func(int(), func(int(), boolean())) => Ok(vec![IR::Eql]),
					"_if_" => todo!(),
					"_else_" => todo!(),
					s => Err(format!("unknown function {:?} with type {:?}", s, t))?,
				},
				Type::Var(_) => unreachable!(),
			}
		},
		&mut |l, xs, _| match l {
			Lists::Paren => xs[0].clone(),
			Lists::Curly => xs
				.into_iter()
				.collect::<Result<Vec<Vec<IR>>, _>>()
				.map(|xs: Vec<Vec<IR>>| xs.into_iter().flatten().collect()),
			Lists::Square => todo!(),
		},
		&mut |f, x, _| {
			let mut out = x?;
			out.extend(f?);
			Ok(out)
		},
	)
}

use Frame::*;
#[derive(Clone, Debug, PartialEq)]
pub enum Frame {
	Int(i64),
	Bool(bool),
}

pub fn interpret(program: &[IR]) -> Vec<Frame> {
	let mut stack = vec![];
	let mut i = 0;
	while i < program.len() {
		if let IR::Push(data) = &program[i] {
			stack.push(data.clone());
		} else {
			match (&program[i], stack.pop().unwrap(), stack.pop().unwrap()) {
				(IR::Add, Int(a), Int(b)) => stack.push(Int(a + b)),
				(IR::Sub, Int(a), Int(b)) => stack.push(Int(a - b)),
				(IR::Mul, Int(a), Int(b)) => stack.push(Int(a * b)),
				(IR::Eql, a, b) => stack.push(Bool(a == b)),
				(op, a, b) => panic!("unknown op {:?} with args {:?} and {:?}", op, a, b),
			}
		}
		i += 1;
	}
	stack
}
