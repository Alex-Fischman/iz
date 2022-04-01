#[derive(Clone, Debug, PartialEq)]
pub enum Lists {
	Curly,
}

pub type TypedAST = crate::tree::Tree<crate::tokenizer::Token, Lists, Type>;
pub type Type = crate::tree::Tree<usize, String, ()>;

pub fn int() -> Type {
	Type::List("int".to_string(), vec![], ())
}

pub fn boolean() -> Type {
	Type::List("bool".to_string(), vec![], ())
}

pub fn string() -> Type {
	Type::List("string".to_string(), vec![], ())
}

pub fn option(t: Type) -> Type {
	Type::List("option".to_string(), vec![t], ())
}

pub fn func(f: Type, x: Type) -> Type {
	Type::Call(Box::new(f), Box::new(x), ())
}

pub fn annotate(ast: &crate::parser::AST) -> Result<TypedAST, String> {
	let mut vars = 0;
	let tree = ast.walk(
		&mut |t: &crate::tokenizer::Token, ()| {
			vars += match &*t.string {
				s if s.chars().next().unwrap().is_numeric() => 0,
				s if s.chars().next().unwrap() == '"' => 0,
				"add" | "sub" | "mul" | "neg" => 1,
				"true" | "false" => 0,
				"eql" | "_if_" | "_else_" => 1,
				"func" => 2,
				_ => 1,
			};
			let var = || Type::Leaf(vars - 1, ());
			Ok(TypedAST::Leaf(
				t.clone(),
				match &*t.string {
					s if s.chars().next().unwrap().is_numeric() => int(),
					s if s.chars().next().unwrap() == '"' => string(),
					"add" | "sub" | "mul" => func(var(), func(var(), var())),
					"neg" => func(var(), var()),
					"true" => boolean(),
					"false" => boolean(),
					"eql" => func(var(), func(var(), boolean())),
					"_if_" => func(boolean(), func(var(), option(var()))),
					"_else_" => func(option(var()), func(var(), var())),
					"func" => func(
						Type::Leaf(vars - 2, ()),
						func(var(), func(Type::Leaf(vars - 2, ()), var())),
					),
					_ => var(),
				},
			))
		},
		&mut |a, xs, ()| match &*a.string {
			"(" => match xs.len() {
				1 => Ok(xs[0].clone()?),
				_ => Err(format!("paren should contain only one expression")),
			},
			"{" => {
				let typed_xs = xs.into_iter().collect::<Result<Vec<_>, _>>()?;
				let t = typed_xs
					.last()
					.map(|x| match x {
						TypedAST::Leaf(_, t) => t.clone(),
						TypedAST::List(_, _, t) => t.clone(),
						TypedAST::Call(_, _, t) => t.clone(),
					})
					.unwrap_or(Type::List("unit".to_string(), vec![], ()));
				Ok(TypedAST::List(Lists::Curly, typed_xs, t))
			}
			"[" => todo!("arrays"),
			s => Err(format!("unknown bracket: {:?}", s)),
		},
		&mut |f, x, ()| match match f.clone()? {
			TypedAST::Leaf(_, t) => t,
			TypedAST::List(_, _, t) => t,
			TypedAST::Call(_, _, t) => t,
		} {
			Type::Call(_, b, ()) => Ok(TypedAST::call(f?, x?, *b.clone())),
			t => Err(format!("expected function type but found {:?}", t)),
		},
	)?;

	let mut constraints = vec![vec![]; vars];
	tree.walk(&mut |_, t| t.clone(), &mut |_, _, t| t.clone(), &mut |f, x, t| match f {
		Type::Call(a, _, ()) => {
			get_constraints(&a, &x, &mut constraints);
			t.clone()
		}
		_ => unreachable!(),
	});
	fn get_constraints(a: &Type, b: &Type, constraints: &mut Vec<Vec<Type>>) {
		match (a, b) {
			(Type::Leaf(i, ()), t) | (t, Type::Leaf(i, ())) => constraints[*i].push(t.clone()),
			(Type::List(a, xs, ()), Type::List(b, ys, ())) if a == b => {
				for (x, y) in xs.iter().zip(ys) {
					get_constraints(x, y, constraints);
				}
			}
			(Type::Call(a, c, ()), Type::Call(b, d, ())) => {
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
			if let Type::Leaf(j, ()) = constraint {
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

	if !unsolved.is_empty() {
		Err(format!(
			"unresolved constraints: {:?}\nvars: {:?}\nast: {:#?}",
			unsolved, solved_vars, tree
		))?
	}

	let update_type = |t: &Type| {
		t.walk(
			&mut |i, ()| solved_vars[*i].clone().unwrap(),
			&mut |s, xs, ()| Type::List(s.to_string(), xs, ()),
			&mut |a, b, ()| Type::Call(Box::new(a), Box::new(b), ()),
		)
	};
	Ok(tree.walk(
		&mut |token, t| TypedAST::Leaf(token.clone(), update_type(t)),
		&mut |l, xs, t| TypedAST::List(l.clone(), xs, update_type(t)),
		&mut |a, b, t| TypedAST::Call(Box::new(a), Box::new(b), update_type(t)),
	))
}
