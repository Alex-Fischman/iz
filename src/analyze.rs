use crate::parse::{parse, Parsed, Tree as ParseTree};
use crate::tokenize::{tokenize, Bracket};
use crate::{Error, Location, PRELUDE_PATH};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
	Int,
	Bool,
	String,
	Group(Vec<Type>),
	Block(Io),
	Option(Box<Type>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Io {
	pub inputs: Vec<Type>,
	pub outputs: Vec<Type>,
}

impl Io {
	fn new(inputs: Vec<Type>, outputs: Vec<Type>) -> Io {
		Io { inputs, outputs }
	}

	fn last(&self) -> Result<&Type, String> {
		self.outputs.last().ok_or_else(|| "no value on stack".to_owned())
	}

	fn combine(&mut self, other: &Io) -> Result<(), String> {
		fn eq(a: &Type, b: &Type) -> Result<(), String> {
			match (a, b) {
				(Type::Int, Type::Int)
				| (Type::Bool, Type::Bool)
				| (Type::String, Type::String) => Ok(()),
				(Type::Group(c), Type::Group(d)) if c.len() == d.len() => {
					c.iter().zip(d).try_fold((), |_, (e, f)| eq(e, f))
				}
				(Type::Block(c), Type::Block(d))
					if c.inputs.len() == d.inputs.len()
						&& c.outputs.len() == d.outputs.len() =>
				{
					c.inputs.iter().zip(&d.inputs).try_fold((), |_, (e, f)| eq(e, f))?;
					c.outputs.iter().zip(&d.outputs).try_fold((), |_, (e, f)| eq(e, f))?;
					Ok(())
				}
				(Type::Option(c), Type::Option(d)) => eq(c, d),
				(a, b) => Err(format!("types aren't equal: {:?}, {:?}", a, b)),
			}
		}
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		let a = self.outputs.drain(i..);
		let b = &other.inputs[j..];
		a.zip(b).try_fold((), |_, (a, b)| eq(&a, b))?;
		let c = &other.inputs[..j];
		self.inputs.extend(c.iter().cloned());
		self.outputs.extend(other.outputs.iter().cloned());
		Ok(())
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tree {
	pub io: Io,
	pub data: Parsed,
	pub location: Location,
	pub children: Vec<Tree>,
}

impl Tree {
	fn new(tree: &ParseTree, io: Io, children: Vec<Tree>) -> Tree {
		Tree { io, data: tree.data.clone(), location: tree.location, children }
	}
}

use std::collections::HashMap;
type Context = Vec<HashMap<String, Vec<Type>>>;
pub fn analyze(trees: &[ParseTree]) -> Result<Vec<Tree>, Error> {
	fn analyze(
		trees: &[ParseTree],
		io: &mut Io,
		context: &mut Context,
	) -> Result<Vec<Tree>, Error> {
		let mut out = vec![];
		for tree in trees {
			let l = tree.location;
			let children;
			let t;
			match &tree.data {
				Parsed::Name(i) if i == "=" => match tree.children.as_slice() {
					s @ [a @ ParseTree { data: Parsed::Name(key), children: cs, .. }, _]
						if cs.is_empty() =>
					{
						let a = Tree::new(a, Io::new(vec![], vec![]), vec![]);
						let b = analyze(&s[1..], io, context)?.remove(0);
						children = vec![a, b];
						let last = io.last().map_err(|s| Error(s, l))?;
						t = Io::new(vec![last.clone()], vec![]);
						let frame =
							match context.iter_mut().find(|frame| frame.contains_key(key)) {
								Some(frame) => frame,
								None => context.last_mut().unwrap(),
							};
						frame.entry(key.to_owned()).or_insert(vec![]).push(last.clone());
					}
					s => {
						return Err(Error(format!("expected name and value, found: {:?}", s), l))
					}
				},
				Parsed::Name(i) if i == "@" => {
					let mut a = Io::new(vec![], vec![]);
					let mut xs = analyze(&tree.children[1..], &mut a, context)?;
					let x = analyze(&tree.children[0..1], &mut a, context)?.remove(0);
					xs.insert(0, x);
					children = xs;
					t = a;
				}
				Parsed::Name(i) => {
					children = analyze(&tree.children, io, context)?;
					let last = || io.last().cloned().map_err(|s| Error(s, l));
					t = match i.as_str() {
						"true" | "false" => Io::new(vec![], vec![Type::Bool]),
						"add" | "sub" | "mul" => {
							Io::new(vec![Type::Int, Type::Int], vec![Type::Int])
						}
						"not" => Io::new(vec![Type::Bool], vec![Type::Bool]),
						"eq" => Io::new(vec![last()?, last()?], vec![Type::Bool]),
						"lt" | "gt" => Io::new(vec![Type::Int, Type::Int], vec![Type::Bool]),
						"call" => match last()? {
							Type::Block(Io { inputs, outputs }) => {
								let mut i = inputs.clone();
								i.push(Type::Block(Io::new(inputs.clone(), outputs.clone())));
								Io::new(i, outputs.clone())
							}
							t => Err(Error(format!("expected block, found {:?}", t), l))?,
						},
						"_if_" => Io::new(
							vec![Type::Bool, last()?],
							vec![Type::Option(Box::new(last()?))],
						),
						"_else_" => Io::new(
							vec![Type::Option(Box::new(last()?)), last()?],
							vec![last()?],
						),
						"_while_" => Io::new(
							vec![
								Type::Block(Io::new(vec![], vec![Type::Bool])),
								Type::Block(Io::new(vec![], vec![])),
							],
							vec![],
						),
						"print" => Io::new(vec![last()?], vec![]),
						key => Io::new(
							vec![],
							vec![match context
								.iter()
								.find_map(|frame| frame.get(key))
								.ok_or_else(|| Error("var not found".to_owned(), l))?
								.as_slice()
							{
								[t] => t.clone(),
								_ => todo!(),
							}],
						),
					};
				}
				Parsed::Number(_) => {
					children = analyze(&tree.children, io, context)?;
					t = Io::new(vec![], vec![Type::Int]);
				}
				Parsed::String(_) => {
					children = analyze(&tree.children, io, context)?;
					t = Io::new(vec![], vec![Type::String]);
				}
				Parsed::Brackets(b) => {
					let mut a = Io::new(vec![], vec![]);
					children = analyze(&tree.children, &mut a, context)?;
					t = match b {
						Bracket::Round => a,
						Bracket::Curly => Io::new(vec![], vec![Type::Block(a)]),
						Bracket::Square => Io::new(a.inputs, vec![Type::Group(a.outputs)]),
					};
				}
			}
			io.combine(&t).map_err(|s| Error(s, l))?;
			out.push(Tree::new(tree, t, children));
		}
		Ok(out)
	}
	let prelude = std::fs::read_to_string(PRELUDE_PATH)
		.map_err(|_| Error("could not read prelude".to_owned(), Location(0, 0)))?
		.chars()
		.collect::<Vec<char>>();
	let mut io = Io::new(vec![], vec![]);
	let mut context = vec![HashMap::new()];
	analyze(&parse(&tokenize(&prelude)?)?, &mut io, &mut context)?;
	let out = analyze(trees, &mut io, &mut context)?;
	if io.inputs.is_empty() {
		Ok(out)
	} else {
		Err(Error(format!("program expected {:?}", io.inputs), Location(0, 0)))
	}
}

#[test]
fn analyze_test() {
	let mut io = Io::new(vec![], vec![Type::Bool]);
	io.combine(&Io::new(vec![Type::Int, Type::Bool], vec![Type::Int])).unwrap();
	assert_eq!(io, Io::new(vec![Type::Int], vec![Type::Int]));
	let f = |s: &str| analyze(&parse(&tokenize(&s.chars().collect::<Vec<char>>())?)?);
	assert_eq!(
		f("1 2 add"),
		Ok(vec![
			Tree {
				io: Io::new(vec![], vec![Type::Int]),
				data: Parsed::Number(1),
				location: Location(0, 1),
				children: vec![]
			},
			Tree {
				io: Io::new(vec![], vec![Type::Int]),
				data: Parsed::Number(2),
				location: Location(2, 1),
				children: vec![]
			},
			Tree {
				io: Io::new(vec![Type::Int, Type::Int], vec![Type::Int]),
				data: Parsed::Name("add".to_owned()),
				location: Location(4, 3),
				children: vec![]
			}
		])
	);
	assert_eq!(
		f("true 2 add"),
		Err(Error("types aren't equal: Bool, Int".to_owned(), Location(7, 3)))
	);
	assert_eq!(f("2 add"), Err(Error("program expected [Int]".to_owned(), Location(0, 0))));
}
