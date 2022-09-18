use crate::parse::Parsed;
use crate::parse::Tree;
use crate::parse::OPERATORS;
use crate::tokenize::Bracket;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Named {
	Name(usize),
	String(String),
	Number(i64),
	Brackets(Bracket),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
	Symbol,
	Int,
	Bool,
	String,
	Block(Vec<Type>, Vec<Type>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Typed(Named, (Vec<Type>, Vec<Type>));

pub fn analyze<'a>(asts: &[Tree<'a, Parsed>]) -> Result<Vec<Tree<'a, Typed>>, String> {
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
		let get_last =
			|v: &[Type]| v.last().cloned().ok_or_else(|| "extra type expected".to_owned());
		let mut out = vec![];
		for tree in named {
			out.push(Tree {
				children: annotate(&tree.children, stack, names)?,
				data: Typed(
					tree.data.clone(),
					match tree.data {
						Named::Name(i) => match names[i].as_str() {
							"true" | "false" => (vec![], vec![Type::Bool]),
							"call" => todo!(),
							"mul" | "div" | "add" | "sub" => {
								(vec![Type::Int, Type::Int], vec![Type::Int])
							}
							"eq" | "ne" => {
								(vec![get_last(stack)?, get_last(stack)?], vec![Type::Bool])
							}
							"lt" | "gt" | "le" | "ge" => todo!(),
							"assign" => todo!(),
							"_if_" => todo!(),
							"_else_" => todo!(),
							"_while_" => (
								vec![
									Type::Block(vec![], vec![Type::Bool]),
									Type::Block(vec![], vec![]),
								],
								vec![],
							),
							_ => (vec![], vec![Type::Symbol]),
						},
						Named::String(_) => (vec![], vec![Type::String]),
						Named::Number(_) => (vec![], vec![Type::Int]),
						Named::Brackets(_) => todo!(),
					},
				),
				location: tree.location,
			});
			let (inputs, outputs) = &out.last().unwrap().data.1;
			for input in inputs {
				if let Some(t) = stack.pop() {
					if *input != t {
						Err(format!("types not equal:\n{:?}\n{:?}", input, t))?
					}
				} else {
					Err(format!("extra type expected: {:?}", input))?
				}
			}
			for output in outputs {
				stack.push(output.clone());
			}
		}
		Ok(out)
	}
	annotate(&named, &mut vec![], &names)
}

#[test]
fn analyze_test() {
	use crate::parse::parse;
	use crate::tokenize::tokenize;
	use crate::tokenize::Location;
	let chars: Vec<char> = "true + 1".chars().collect();
	assert_eq!(
		analyze(&parse(&tokenize(&chars).unwrap()).unwrap()),
		Err("types not equal:\nInt\nBool".to_owned())
	);
	let chars: Vec<char> = "1 add".chars().collect();
	assert_eq!(
		analyze(&parse(&tokenize(&chars).unwrap()).unwrap()),
		Err("extra type expected: Int".to_owned())
	);
	let chars: Vec<char> = "eq".chars().collect();
	assert_eq!(
		analyze(&parse(&tokenize(&chars).unwrap()).unwrap()),
		Err("extra type expected".to_owned())
	);
	let chars: Vec<char> = "1 2 add".chars().collect();
	assert_eq!(
		analyze(&parse(&tokenize(&chars).unwrap()).unwrap()),
		Ok(vec![
			Tree {
				data: Typed(Named::Number(1), (vec![], vec![Type::Int])),
				location: Location(0, 1, &chars),
				children: vec![]
			},
			Tree {
				data: Typed(Named::Number(2), (vec![], vec![Type::Int])),
				location: Location(2, 1, &chars),
				children: vec![]
			},
			Tree {
				data: Typed(Named::Name(0), (vec![Type::Int, Type::Int], vec![Type::Int])),
				location: Location(4, 3, &chars),
				children: vec![]
			},
		])
	);
	let chars: Vec<char> = "1 + 2".chars().collect();
	assert_eq!(
		analyze(&parse(&tokenize(&chars).unwrap()).unwrap()),
		Ok(vec![Tree {
			data: Typed(Named::Name(0), (vec![Type::Int, Type::Int], vec![Type::Int])),
			location: Location(2, 1, &chars),
			children: vec![
				Tree {
					data: Typed(Named::Number(1), (vec![], vec![Type::Int])),
					location: Location(0, 1, &chars),
					children: vec![]
				},
				Tree {
					data: Typed(Named::Number(2), (vec![], vec![Type::Int])),
					location: Location(4, 1, &chars),
					children: vec![]
				},
			]
		}])
	);
}
