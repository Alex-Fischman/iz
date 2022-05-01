use crate::parser::Lists;
use crate::typer::{Type, TypedAST};
use std::io::{Error, ErrorKind};

#[derive(Debug, PartialEq)]
pub enum Op {
	PushI(i64),
	NegI,
	AddI,
	SubI,
	MulI,
	EqlI,
	PushB(bool),
	EqlB,
	Jump,
	Ip,
	Return,
}

pub fn compile(ast: &TypedAST) -> Result<Vec<Op>, Error> {
	Ok(match ast {
		TypedAST::Leaf(token, t) => match token.string.as_str() {
			"neg" if t == &(vec![Type::Int], vec![Type::Int]) => vec![Op::NegI],
			"add" if t == &(vec![Type::Int, Type::Int], vec![Type::Int]) => vec![Op::AddI],
			"sub" if t == &(vec![Type::Int, Type::Int], vec![Type::Int]) => vec![Op::SubI],
			"mul" if t == &(vec![Type::Int, Type::Int], vec![Type::Int]) => vec![Op::MulI],
			"eql" if t == &(vec![Type::Int, Type::Int], vec![Type::Bool]) => vec![Op::EqlI],
			"true" if t == &(vec![], vec![Type::Bool]) => vec![Op::PushB(true)],
			"false" if t == &(vec![], vec![Type::Bool]) => vec![Op::PushB(false)],
			"eql" if t == &(vec![Type::Bool, Type::Bool], vec![Type::Bool]) => vec![Op::EqlB],
			s if t == &(vec![], vec![Type::Int]) && s.chars().next().unwrap().is_numeric() => {
				vec![Op::PushI(s.parse::<i64>().unwrap())]
			}
			s => Err(Error::new(ErrorKind::Other, format!("unknown op {:?} {:?}", s, t)))?,
		},
		TypedAST::List(l, xs, _) => {
			let code: Vec<Op> = xs
				.iter()
				.map(|x| compile(x))
				.collect::<Result<Vec<Vec<Op>>, Error>>()?
				.into_iter()
				.flatten()
				.collect();
			match l {
				None => code,
				Some(Lists::Group) => todo!(),
				Some(Lists::Block) => {
					let code_len = code.len() as i64;
					let mut block = vec![];
					block.extend([Op::PushI(1 + code_len), Op::Jump]);
					block.extend(code);
					block.extend([Op::Return, Op::Ip, Op::PushI(1 + code_len), Op::SubI]);
					block
				}
			}
		}
	})
}
