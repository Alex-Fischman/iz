use crate::parser::AST;
use crate::tokenizer::Token;

#[derive(Debug)]
pub enum TypedAST {
	Token(Token, Type),
	List(Token, Vec<TypedAST>, Token, Type),
	Call(Box<TypedAST>, Box<TypedAST>, Type),
}

impl TypedAST {
	pub fn call(f: TypedAST, x: TypedAST, t: Type) -> TypedAST {
		TypedAST::Call(Box::new(f), Box::new(x), t)
	}

	pub fn get_type(&self) -> &Type {
		match self {
			TypedAST::Token(_, t) => t,
			TypedAST::Call(_, _, t) => t,
			TypedAST::List(_, _, _, t) => t,
		}
	}

	fn set_type(&mut self, u: Type) {
		match self {
			TypedAST::Token(_, t) => *t = u,
			TypedAST::Call(_, _, t) => *t = u,
			TypedAST::List(_, _, _, t) => *t = u,
		}
	}
}

#[derive(Clone, PartialEq)]
pub enum Type {
	Data(String, Vec<Type>),
	Func(Box<Type>, Box<Type>),
	Var(usize),
}

impl Type {
	pub fn data(s: &str) -> Type {
		Type::Data(s.to_string(), vec![])
	}

	pub fn func(f: Type, x: Type) -> Type {
		Type::Func(Box::new(f), Box::new(x))
	}
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
	let mut num_vars = 0;
	let mut tree = generate_tree(ast, &mut num_vars)?;
	fn generate_tree(ast: &AST, num_vars: &mut usize) -> Result<TypedAST, String> {
		let option = |a| Type::Data("option".to_string(), vec![a]);
		let cur_var = |num_vars: &mut usize| Type::Var(*num_vars - 1);
		let mut new_var = || {
			*num_vars += 1;
			cur_var(num_vars)
		};

		match ast {
			AST::Token(t) => Ok(TypedAST::Token(
				t.clone(),
				match &*t.string {
					s if s.chars().next().unwrap().is_numeric() => Type::data("int"),
					s if s.chars().next().unwrap() == '"' => Type::data("str"),
					"add" => Type::func(
						Type::data("int"),
						Type::func(Type::data("int"), Type::data("int")),
					),
					"sub" => Type::func(
						Type::data("int"),
						Type::func(Type::data("int"), Type::data("int")),
					),
					"mul" => Type::func(
						Type::data("int"),
						Type::func(Type::data("int"), Type::data("int")),
					),
					"neg" => Type::func(Type::data("int"), Type::data("int")),
					"true" => Type::data("bool"),
					"false" => Type::data("bool"),
					"_if_" => Type::func(
						Type::data("bool"),
						Type::func(new_var(), option(cur_var(num_vars))),
					),
					"_else_" => Type::func(
						option(new_var()),
						Type::func(cur_var(num_vars), cur_var(num_vars)),
					),
					_ => Err(format!("unknown token: {:?}", t))?,
				},
			)),
			AST::List(a, xs, b) => match &*a.string {
				"(" => match xs.get(0) {
					Some(x) if xs.len() == 1 => Ok(generate_tree(x, num_vars)?),
					_ => Err(format!("paren should only contain one expression")),
				},
				"{" => {
					let mut typed_xs = vec![];
					for x in xs {
						typed_xs.push(generate_tree(x, num_vars)?);
					}
					let t = typed_xs
						.last()
						.map(|x| x.get_type().clone())
						.unwrap_or(Type::data("unit"));
					Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
				}
				"[" => todo!("arrays"),
				s => Err(format!("unknown bracket: {:?}", s)),
			},
			AST::Call(f, x) => {
				let f = generate_tree(f, num_vars)?;
				match f.get_type().clone() {
					Type::Func(_a, b) => {
						Ok(TypedAST::call(f, generate_tree(x, num_vars)?, *b.clone()))
					}
					t => Err(format!("expected function type but found {:?}", t)),
				}
			}
		}
	}

	let mut constraints = vec![vec![]; num_vars];
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

	let mut vars: Vec<Option<Type>> = vec![None; num_vars];
	let mut unsolved: Vec<(usize, usize)> = vec![];
	for (i, var) in constraints.into_iter().enumerate() {
		for constraint in var {
			if let Type::Var(j) = constraint {
				unsolved.push((i, j));
			} else if let Some(t) = &vars[i] {
				if t != &constraint {
					Err(format!("type error:\n{:?}\n{:?}\n", t, constraint))?;
				}
			} else {
				vars[i] = Some(constraint);
			}
		}
	}

	let mut last_len = unsolved.len() + 1;
	while last_len > unsolved.len() && !unsolved.is_empty() {
		last_len = unsolved.len();

		let mut still_unsolved = vec![];
		for (i, j) in unsolved {
			match (&vars[i], &vars[j]) {
				(Some(a), Some(b)) if a != b => Err(format!("type error:\n{:?}\n{:?}\n", a, b))?,
				(Some(_), Some(_)) => (),
				(None, Some(b)) => vars[i] = Some(b.clone()),
				(Some(a), None) => vars[j] = Some(a.clone()),
				(None, None) => still_unsolved.push((i, j)),
			}
		}
		unsolved = still_unsolved;
	}

	if !unsolved.is_empty() {
		Err(format!("unresolved type constraints"))?
	}

	let vars: Vec<Type> = vars.into_iter().map(Option::unwrap).collect();

	update_tree(&mut tree, &vars);

	fn update_tree(ast: &mut TypedAST, vars: &[Type]) {
		ast.set_type(update_type(ast.get_type(), vars));
		match ast {
			TypedAST::Token(_, _) => {}
			TypedAST::List(_, xs, _, _) => xs.iter_mut().for_each(|x| update_tree(x, vars)),
			TypedAST::Call(a, b, _) => {
				update_tree(a, vars);
				update_tree(b, vars);
			}
		}
	}
	fn update_type(t: &Type, vars: &[Type]) -> Type {
		match t {
			Type::Var(i) => vars[*i].clone(),
			Type::Data(s, xs) => {
				Type::Data(s.to_string(), xs.iter().map(|x| update_type(x, vars)).collect())
			}
			Type::Func(a, b) => {
				Type::Func(Box::new(update_type(a, vars)), Box::new(update_type(b, vars)))
			}
		}
	}

	Ok(tree)
}
