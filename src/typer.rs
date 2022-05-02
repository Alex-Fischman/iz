use crate::parser::*;
use std::io::{Error, ErrorKind};

#[derive(PartialEq)]
pub enum TypedAST {
	Leaf(crate::tokenizer::Token, (Vec<Type>, Vec<Type>)),
	List(Lists, Vec<TypedAST>, (Vec<Type>, Vec<Type>)),
}

#[derive(Clone, Debug)]
pub enum Type {
	Int,
	Bool,
	Var(usize),
	Block(Vec<Type>, Vec<Type>),
}

impl PartialEq for Type {
	fn eq(&self, other: &Type) -> bool {
		match (self, other) {
			(Type::Int, Type::Int) => true,
			(Type::Bool, Type::Bool) => true,
			(Type::Var(_), Type::Var(_)) => unreachable!(),
			(Type::Block(a, c), Type::Block(b, d)) => {
				a.iter().zip(b).all(|(a, b)| a == b) && c.iter().zip(d).all(|(c, d)| c == d)
			}
			_ => false,
		}
	}
}

impl std::fmt::Debug for TypedAST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TypedAST::Leaf(token, t) => write!(f, "{:?}:{:?}", token, t),
			TypedAST::List(l, v, t) => write!(f, "{:?}{:#?}:{:?}", l, v, t),
		}
	}
}

fn equalize_types(a: &Type, b: &Type, vars: &mut Vec<Option<Type>>) -> Result<(), Error> {
	Ok(match (a, b) {
		(Type::Var(u), Type::Var(v)) => match (vars[*u].clone(), vars[*v].clone()) {
			(None, None) => todo!(),
			(Some(a), None) => vars[*v] = Some(a.clone()),
			(None, Some(b)) => vars[*u] = Some(b.clone()),
			(Some(a), Some(b)) => equalize_types(&a, &b, vars)?,
		},
		(a, Type::Var(v)) | (Type::Var(v), a) => match (a, vars[*v].clone()) {
			(a, None) => vars[*v] = Some(a.clone()),
			(a, Some(b)) => equalize_types(&a, &b, vars)?,
		},
		(Type::Block(a, c), Type::Block(b, d)) => {
			for (a, b) in a.iter().zip(b) {
				equalize_types(a, b, vars)?;
			}
			for (c, d) in c.iter().zip(d) {
				equalize_types(c, d, vars)?;
			}
		}
		(a, b) => match a == b {
			true => {}
			false => {
				Err(Error::new(ErrorKind::Other, format!("type error: {:?} != {:?}", a, b)))?
			}
		},
	})
}

pub fn annotate(ast: &AST) -> Result<TypedAST, Error> {
	fn annotate(ast: &AST, vars: &mut Vec<Option<Type>>) -> Result<TypedAST, Error> {
		match ast {
			AST::Leaf(token) => Ok(TypedAST::Leaf(
				token.clone(),
				match token.string.as_str() {
					s if s.chars().next().unwrap().is_numeric() => (vec![], vec![Type::Int]),
					"true" | "false" => (vec![], vec![Type::Bool]),
					"add" | "sub" | "mul" => (vec![Type::Int, Type::Int], vec![Type::Int]),
					"neg" => (vec![Type::Int], vec![Type::Int]),
					"eql" => (
						vec![
							Type::Var({
								vars.push(None);
								vars.len() - 1
							}),
							Type::Var(vars.len() - 1),
						],
						vec![Type::Bool],
					),
					"call" => todo!(),
					t => Err(Error::new(ErrorKind::Other, format!("unknown token {:?}", t)))?,
				},
			)),
			AST::List(l, xs) => {
				let typed_xs = xs
					.iter()
					.map(|x| annotate(x, vars))
					.collect::<Result<Vec<TypedAST>, Error>>()?;
				let mut input_stack = vec![];
				let mut output_stack = vec![];
				for x in &typed_xs {
					let (input, output) = match x {
						TypedAST::Leaf(_, t) => t,
						TypedAST::List(_, _, t) => t,
					};
					let mut input = input.clone();
					while !output_stack.is_empty() && !input.is_empty() {
						equalize_types(&output_stack.pop().unwrap(), &input.remove(0), vars)?;
					}
					input_stack.extend(input);
					output_stack.extend(output.clone());
				}
				Ok(TypedAST::List(
					*l,
					typed_xs,
					match l {
						Lists::Group => (input_stack, output_stack),
						Lists::Block => (vec![], vec![Type::Block(input_stack, output_stack)]),
					},
				))
			}
		}
	}

	let mut vars = vec![];
	let mut out = annotate(ast, &mut vars)?;
	for var in &vars {
		if var.is_none() {
			Err(Error::new(ErrorKind::Other, format!("type error: unsolved var")))?
		}
	}
	fn replace_vars_in_ast(ast: &mut TypedAST, vars: &Vec<Type>) {
		if let TypedAST::List(_, xs, _) = ast {
			xs.iter_mut().for_each(|x| replace_vars_in_ast(x, vars));
		}
		let (input, output) = &mut match ast {
			TypedAST::Leaf(_, t) => t,
			TypedAST::List(_, _, t) => t,
		};
		replace_vars_in_type(input, vars);
		replace_vars_in_type(output, vars);
	}
	fn replace_vars_in_type(ts: &mut Vec<Type>, vars: &Vec<Type>) {
		for t in ts {
			match t {
				Type::Int => {}
				Type::Bool => {}
				Type::Var(v) => *t = vars[*v].clone(),
				Type::Block(a, b) => {
					replace_vars_in_type(a, vars);
					replace_vars_in_type(b, vars);
				}
			}
		}
	}
	replace_vars_in_ast(&mut out, &vars.into_iter().map(Option::unwrap).collect());
	Ok(out)
}
