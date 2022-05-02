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
	Call,
	Return,
}

pub fn compile(ast: &TypedAST) -> Result<Vec<Op>, Error> {
	Ok(match ast {
		TypedAST::Leaf(token, t) => match token.string.as_str() {
			"neg" if t == &(Type::Int, Type::Int) => vec![Op::NegI],
			"add" if t == &(Type::two_ints(), Type::Int) => vec![Op::AddI],
			"sub" if t == &(Type::two_ints(), Type::Int) => vec![Op::SubI],
			"mul" if t == &(Type::two_ints(), Type::Int) => vec![Op::MulI],
			"eql" if t == &(Type::two_ints(), Type::Bool) => vec![Op::EqlI],
			"true" if t == &(Type::unit(), Type::Bool) => vec![Op::PushB(true)],
			"false" if t == &(Type::unit(), Type::Bool) => vec![Op::PushB(false)],
			"eql" if t == &(Type::Group(vec![Type::Bool, Type::Bool]), Type::Bool) => {
				vec![Op::EqlB]
			}
			"call" => vec![Op::Call],
			s if t == &(Type::unit(), Type::Int) && s.chars().next().unwrap().is_numeric() => {
				vec![Op::PushI(s.parse::<i64>().unwrap())]
			}
			s => Err(Error::new(ErrorKind::Other, format!("unknown op {} {:?}", s, t)))?,
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
				Lists::Group => code,
				Lists::Block => {
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
