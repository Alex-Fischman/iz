use crate::parser::AST;
use crate::tokenizer::Token;

#[derive(Debug)]
pub enum TypedAST {
	Token(Token, Type),
	List(Token, Vec<TypedAST>, Token, Type),
	Call(Box<TypedAST>, Box<TypedAST>, Type),
}

impl TypedAST {
	fn get_type(&self) -> &Type {
		match self {
			TypedAST::Token(_, t) => t,
			TypedAST::Call(_, _, t) => t,
			TypedAST::List(_, _, _, t) => t,
		}
	}
}

#[derive(Clone, PartialEq)]
pub enum Type {
	Data(String, Vec<Type>),
	Func(Box<Type>, Box<Type>),
	Var(usize),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Var(i) => write!(f, "v{:?}", i),
			Type::Func(x, y) => write!(f, "{:?}->{:?}", x, y),
			Type::Data(s, v) => {
				write!(f, "{}", s)?;
				for x in v {
					write!(f, "@{:?}", x)?;
				}
				Ok(())
			}
		}
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let mut vars = 0;
	let tree = generate_tree(ast, &mut vars)?;
	fn generate_tree(ast: &AST, vars: &mut usize) -> Result<TypedAST, String> {
		let data = |s: &str| Type::Data(s.to_string(), vec![]);
		let func = |x, y| Type::Func(Box::new(x), Box::new(y));
		let option = |a| Type::Data("option".to_string(), vec![a]);
		let cur_var = |vars: &mut usize| Type::Var(*vars - 1);
		let mut new_var = || {
			*vars += 1;
			cur_var(vars)
		};

		match ast {
			AST::Token(t) => Ok(TypedAST::Token(
				t.clone(),
				match &*t.string {
					s if s.chars().next().unwrap().is_numeric() => data("int"),
					"_iadd_" => func(data("int"), func(data("int"), data("int"))),
					"_isub_" => func(data("int"), func(data("int"), data("int"))),
					"_imul_" => func(data("int"), func(data("int"), data("int"))),
					"_ineg_" => func(data("int"), data("int")),
					"true" => data("bool"),
					"false" => data("bool"),
					"_if_" => func(data("bool"), func(new_var(), option(cur_var(vars)))),
					"_else_" => func(option(new_var()), func(cur_var(vars), cur_var(vars))),
					_ => Err(format!("unknown token: {:?}", t))?,
				},
			)),
			AST::List(a, xs, b) => match &*a.string {
				"(" => match xs.get(0) {
					Some(x) if xs.len() == 1 => Ok(generate_tree(x, vars)?),
					_ => Err(format!("paren should only contain one expression")),
				},
				"{" => {
					let mut typed_xs = vec![];
					for x in xs {
						typed_xs.push(generate_tree(x, vars)?);
					}
					let t =
						typed_xs.last().map(|x| x.get_type().clone()).unwrap_or(data("unit"));
					Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
				}
				"[" => todo!("arrays"),
				s => Err(format!("unknown bracket: {:?}", s)),
			},
			AST::Call(f, x) => {
				let f = generate_tree(f, vars)?;
				match f.get_type().clone() {
					Type::Func(_a, b) => Ok(TypedAST::Call(
						Box::new(f),
						Box::new(generate_tree(x, vars)?),
						*b.clone(),
					)),
					t => Err(format!("expected function type but found {:?}", t)),
				}
			}
		}
	}

	let mut constraints = vec![vec![]; vars];
	find_constraints(&tree, &mut constraints)?;
	fn find_constraints(ast: &TypedAST, constraints: &mut Vec<Vec<Type>>) -> Result<(), String> {
		Ok(match ast {
			TypedAST::Token(_, _) => (),
			TypedAST::List(_, xs, _, _) => {
				for x in xs {
					find_constraints(x, constraints)?;
				}
			}
			TypedAST::Call(f, x, _) => {
				find_constraints(f, constraints)?;
				find_constraints(x, constraints)?;
				match f.get_type() {
					Type::Func(a, _) => get_constraints(a, x.get_type(), constraints)?,
					_ => unreachable!(),
				}
			}
		})
	}
	fn get_constraints(
		a: &Type,
		b: &Type,
		constraints: &mut Vec<Vec<Type>>,
	) -> Result<(), String> {
		Ok(match (a, b) {
			(Type::Var(i), t) | (t, Type::Var(i)) => constraints[*i].push(t.clone()),
			(Type::Data(a, xs), Type::Data(b, ys)) => {
				if a == b {
					for (x, y) in xs.iter().zip(ys) {
						get_constraints(x, y, constraints)?;
					}
				}
			}
			(Type::Func(a, c), Type::Func(b, d)) => {
				get_constraints(a, b, constraints)?;
				get_constraints(c, d, constraints)?;
			}
			(a, b) => Err(format!("type error:\n{:?}\n{:?}\n", a, b))?,
		})
	}

	// solve constraints
	{
		println!("{:?}", constraints);
		// split constraints into var-type and var-var
		// find a type for as many vars as possible
		// check that all var-var are satisfied
		// check that all vars have a type
	}

	// update tree

	Ok(tree)
}
