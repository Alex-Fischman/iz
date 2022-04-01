use crate::parser::AST;
use crate::tokenizer::Token;
use crate::tree::Tree;

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub enum Lists {
	Paren,
	Curly,
	Square,
}

pub type TypedAST = Tree<Token, Lists, Type>;

impl TypedAST {
	pub fn call(f: TypedAST, x: TypedAST, t: Type) -> TypedAST {
		TypedAST::Call(Box::new(f), Box::new(x), t)
	}

	pub fn get_type(&self) -> &Type {
		match self {
			TypedAST::Leaf(_, t) => t,
			TypedAST::Call(_, _, t) => t,
			TypedAST::List(_, _, t) => t,
		}
	}

	fn set_type(&mut self, u: Type) {
		match self {
			TypedAST::Leaf(_, t) => *t = u,
			TypedAST::Call(_, _, t) => *t = u,
			TypedAST::List(_, _, t) => *t = u,
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

pub fn int() -> Type {
	Type::Data("int".to_string(), vec![])
}

pub fn boolean() -> Type {
	Type::Data("bool".to_string(), vec![])
}

pub fn string() -> Type {
	Type::Data("string".to_string(), vec![])
}

pub fn option(t: Type) -> Type {
	Type::Data("option".to_string(), vec![t])
}

pub fn func(f: Type, x: Type) -> Type {
	Type::Func(Box::new(f), Box::new(x))
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let mut vars = 0;
	use Type::Var;

	let mut tree = ast.walk(
		&mut |t: &Token, ()| {
			Ok(TypedAST::Leaf(
				t.clone(),
				match &*t.string {
					s if s.chars().next().unwrap().is_numeric() => int(),
					s if s.chars().next().unwrap() == '"' => string(),
					"add" | "sub" | "mul" => {
						vars += 1;
						func(Var(vars - 1), func(Var(vars - 1), Var(vars - 1)))
					}
					"neg" => {
						vars += 1;
						func(Var(vars - 1), Var(vars - 1))
					}
					"true" => boolean(),
					"false" => boolean(),
					"eql" => {
						vars += 1;
						func(Var(vars - 1), func(Var(vars - 1), boolean()))
					}
					"_if_" => {
						vars += 1;
						func(boolean(), func(Var(vars - 1), option(Var(vars - 1))))
					}
					"_else_" => {
						vars += 1;
						func(option(Var(vars - 1)), func(Var(vars - 1), Var(vars - 1)))
					}
					"func" => {
						vars += 2;
						func(
							Var(vars - 2),
							func(Var(vars - 1), func(Type::Var(vars - 2), Var(vars - 1))),
						)
					}
					_ => {
						vars += 1;
						Var(vars - 1)
					}
				},
			))
		},
		&mut |a, xs, ()| match &*a.string {
			"(" => match xs.len() {
				1 => Ok(xs[0].clone()?),
				_ => Err(format!("paren should contain only one expression")),
			},
			"{" => {
				let mut typed_xs = vec![];
				for x in xs {
					typed_xs.push(x?);
				}
				let t = typed_xs
					.last()
					.map(|x| x.get_type().clone())
					.unwrap_or(Type::Data("unit".to_string(), vec![]));
				Ok(TypedAST::List(Lists::Curly, typed_xs, t))
			}
			"[" => todo!("arrays"),
			s => Err(format!("unknown bracket: {:?}", s)),
		},
		&mut |f, x, ()| match f.clone()?.get_type().clone() {
			Type::Func(_, b) => Ok(TypedAST::call(f?, x?, *b.clone())),
			t => Err(format!("expected function type but found {:?}", t)),
		},
	)?;

	let mut constraints = vec![vec![]; vars];
	tree.walk(&mut |_, t| t.clone(), &mut |_, _, t| t.clone(), &mut |f, x, t| match f {
		Type::Func(a, _) => {
			get_constraints(&a, &x, &mut constraints);
			t.clone()
		}
		_ => unreachable!(),
	});
	fn get_constraints(a: &Type, b: &Type, constraints: &mut Vec<Vec<Type>>) {
		match (a, b) {
			(Type::Var(i), t) | (t, Type::Var(i)) => constraints[*i].push(t.clone()),
			(Type::Data(a, xs), Type::Data(b, ys)) => {
				if a == b {
					for (x, y) in xs.iter().zip(ys) {
						get_constraints(x, y, constraints);
					}
				}
			}
			(Type::Func(a, c), Type::Func(b, d)) => {
				get_constraints(a, b, constraints);
				get_constraints(c, d, constraints);
			}
			_ => unreachable!(),
		}
	}

	let mut solved_vars: Vec<Option<Type>> = vec![None; vars];
	let mut unsolved: Vec<(usize, usize)> = vec![];
	for (i, var) in constraints.into_iter().enumerate() {
		for constraint in var {
			if let Type::Var(j) = constraint {
				unsolved.push((i, j));
			} else if let Some(t) = &solved_vars[i] {
				if t != &constraint {
					Err(format!("type error:\n{:?}\n{:?}\n", t, constraint))?;
				}
			} else {
				solved_vars[i] = Some(constraint);
			}
		}
	}

	let mut last_len = unsolved.len() + 1;
	while last_len > unsolved.len() && !unsolved.is_empty() {
		last_len = unsolved.len();
		let mut still_unsolved = vec![];
		for (i, j) in unsolved {
			match (&solved_vars[i], &solved_vars[j]) {
				(Some(a), Some(b)) if a != b => Err(format!("a != b:\n{:?}\n{:?}\n", a, b))?,
				(Some(_), Some(_)) => (),
				(None, Some(b)) => solved_vars[i] = Some(b.clone()),
				(Some(a), None) => solved_vars[j] = Some(a.clone()),
				(None, None) => still_unsolved.push((i, j)),
			}
		}
		unsolved = still_unsolved;
	}

	if !unsolved.is_empty() || solved_vars.iter().any(Option::is_none) {
		Err(format!(
			"unresolved constraints: {:?}\nvars: {:?}\nast: {:#?}",
			unsolved, solved_vars, tree
		))?
	}

	let vars: Vec<Type> = solved_vars.into_iter().map(Option::unwrap).collect();

	update_tree(&mut tree, &vars);

	fn update_tree(ast: &mut TypedAST, vars: &[Type]) {
		ast.set_type(update_type(ast.get_type(), vars));
		match ast {
			TypedAST::Leaf(_, _) => {}
			TypedAST::List(_, xs, _) => xs.iter_mut().for_each(|x| update_tree(x, vars)),
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
