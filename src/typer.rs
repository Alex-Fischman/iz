use crate::parser::*;
use std::io::{Error, ErrorKind};

#[derive(PartialEq)]
pub enum TypedAST {
	Leaf(crate::tokenizer::Token, (Type, Type)),
	List(Lists, Vec<TypedAST>, (Type, Type)),
}

#[derive(Clone, Debug)]
pub enum Type {
	Int,
	Bool,
	Var(usize),
	Block(Box<Type>, Box<Type>),
	Group(Vec<Type>),
}

impl Type {
	pub fn unit() -> Type {
		Type::Group(vec![])
	}

	pub fn two_ints() -> Type {
		Type::Group(vec![Type::Int, Type::Int])
	}
}

impl PartialEq for Type {
	fn eq(&self, other: &Type) -> bool {
		match (self, other) {
			(Type::Group(a), b) | (b, Type::Group(a)) if a.len() == 1 => &a[0] == b,
			(Type::Int, Type::Int) => true,
			(Type::Bool, Type::Bool) => true,
			(Type::Var(_), Type::Var(_)) => unreachable!(),
			(Type::Block(a, c), Type::Block(b, d)) => a == b && c == d,
			(Type::Group(a), Type::Group(b)) => a.iter().zip(b).all(|(a, b)| a == b),
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
		(Type::Group(a), b) | (b, Type::Group(a)) if a.len() == 1 => {
			equalize_types(&a[0], b, vars)?;
		}
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
			equalize_types(a, b, vars)?;
			equalize_types(c, d, vars)?;
		}
		(Type::Group(a), Type::Group(b)) => {
			for (a, b) in a.iter().zip(b) {
				equalize_types(a, b, vars)?;
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
					s if s.chars().next().unwrap().is_numeric() => (Type::unit(), Type::Int),
					"true" | "false" => (Type::unit(), Type::Bool),
					"add" | "sub" | "mul" => (Type::two_ints(), Type::Int),
					"neg" => (Type::Int, Type::Int),
					"eql" => (
						Type::Group(vec![
							Type::Var({
								vars.push(None);
								vars.len() - 1
							}),
							Type::Var(vars.len() - 1),
						]),
						Type::Bool,
					),
					"call" => {
						vars.push(None);
						vars.push(None);
						(
							Type::Group(vec![
								Type::Var(vars.len() - 1),
								Type::Block(
									Box::new(Type::Var(vars.len() - 1)),
									Box::new(Type::Var(vars.len() - 2)),
								),
							]),
							Type::Var(vars.len() - 2),
						)
					}
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
					fn flatten(t: &Type) -> Vec<Type> {
						if let Type::Group(ts) = t {
							ts.iter().map(flatten).flatten().collect()
						} else {
							vec![t.clone()]
						}
					}
					let mut input = flatten(input);
					'a: while !output_stack.is_empty() && !input.is_empty() {
						for i in (0..output_stack.len()).rev() {
							for j in 1..=input.len() {
								if let Ok(()) = equalize_types(
									&Type::Group(output_stack[i..].to_vec()),
									&Type::Group(input[..j].to_vec()),
									vars,
								) {
									output_stack.splice(i.., []);
									input.splice(..j, []);
									continue 'a;
								}
							}
						}
						break;
					}
					input_stack.extend(input);
					output_stack.extend(flatten(output));
				}
				Ok(TypedAST::List(
					*l,
					typed_xs,
					match l {
						Lists::Group => (Type::Group(input_stack), Type::Group(output_stack)),
						Lists::Block => (
							Type::unit(),
							Type::Block(
								Box::new(Type::Group(input_stack)),
								Box::new(Type::Group(output_stack)),
							),
						),
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
	fn replace_vars_in_type(t: &mut Type, vars: &Vec<Type>) {
		match t {
			Type::Int => {}
			Type::Bool => {}
			Type::Var(v) => *t = vars[*v].clone(),
			Type::Block(a, b) => {
				replace_vars_in_type(a, vars);
				replace_vars_in_type(b, vars);
			}
			Type::Group(xs) => xs.iter_mut().for_each(|x| replace_vars_in_type(x, vars)),
		}
	}
	replace_vars_in_ast(&mut out, &vars.into_iter().map(Option::unwrap).collect());
	Ok(out)
}
