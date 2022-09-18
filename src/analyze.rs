use crate::parse::Parsed;
use crate::parse::Tree;
use crate::parse::OPERATORS;
use crate::tokenize::Bracket;

#[derive(Clone, Debug)]
pub enum Named {
	Name(usize),
	String(String),
	Number(i64),
	Brackets(Bracket),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
	Int,
	Bool,
	String,
}

#[derive(Debug)]
pub struct Typed(Named, (Vec<Type>, Vec<Type>));

pub fn analyze<'a>(
	asts: &[Tree<'a, Parsed>],
) -> Result<(Vec<Tree<'a, Typed>>, Vec<String>), String> {
	fn name<'a>(asts: &[Tree<'a, Parsed>], names: &mut Vec<String>) -> Vec<Tree<'a, Named>> {
		fn new_name(name: &str, names: &mut Vec<String>) -> Named {
			Named::Name(names.iter().position(|n| n == name).unwrap_or_else(|| {
				names.push(name.to_owned());
				names.len() - 1
			}))
		}
		asts.iter()
			.map(|ast| Tree {
				data: match &ast.data {
					Parsed::Ident => {
						new_name(&ast.location.to_chars().iter().collect::<String>(), names)
					}
					Parsed::String(s) => Named::String(s.clone()),
					Parsed::Number(n) => Named::Number(*n),
					Parsed::Brackets(b) => Named::Brackets(*b),
					Parsed::Operator(i, j) => new_name(OPERATORS[*i].0[*j].1, names),
				},
				location: ast.location,
				children: name(&ast.children, names),
			})
			.collect()
	}
	let mut names = vec![];
	let named = name(asts, &mut names);
	fn annotate<'a>(
		named: &[Tree<'a, Named>],
		stack: &mut Vec<Type>,
		names: &[String],
	) -> Result<Vec<Tree<'a, Typed>>, String> {
		let mut out = vec![];
		for tree in named {
			out.push(Tree {
				data: Typed(
					tree.data.clone(),
					match tree.data {
						Named::Name(i) => match names[i].as_str() {
							"true" | "false" => (vec![], vec![Type::Bool]),
							_ => todo!(),
						},
						Named::String(_) => (vec![], vec![Type::String]),
						Named::Number(_) => (vec![], vec![Type::Int]),
						Named::Brackets(_) => todo!(),
					},
				),
				location: tree.location,
				children: annotate(&tree.children, stack, names)?,
			});
			let (inputs, outputs) = &out.last().unwrap().data.1;
			for input in inputs {
				if let Some(t) = stack.pop() {
					if *input != t {
						Err(format!("types not equal:\n{:?}\n{:?}", input, t))?
					}
				} else {
					Err(format!("extra type expected on stack: {:?}", input))?
				}
			}
			for output in outputs {
				stack.push(output.clone());
			}
		}
		Ok(out)
	}
	let mut stack = vec![];
	let typed = annotate(&named, &mut stack, &names)?;
	if stack.is_empty() {
		Ok((typed, names))
	} else {
		Err(format!("extra types remained stack: {:?}", stack))
	}
}
