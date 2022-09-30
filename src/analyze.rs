use crate::parse::{parse, Parsed, Tree as ParseTree};
use crate::tokenize::{tokenize, Bracket};
use crate::{Error, Location, PRELUDE_PATH};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
	Int,
	Bool,
	String,
	Group(Vec<usize>),
	Block(Io),
	Option(usize),
	Unknown,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Io {
	pub inputs: Vec<usize>,
	pub outputs: Vec<usize>,
}

impl Io {
	pub fn new(inputs: Vec<usize>, outputs: Vec<usize>) -> Io {
		Io { inputs, outputs }
	}

	fn last(&self, l: Location) -> Result<usize, Error> {
		self.outputs.last().cloned().ok_or_else(|| Error("no value on stack".to_owned(), l))
	}

	fn combine(&mut self, other: &mut Io, types: &mut [Type]) -> Result<(), String> {
		fn eq(a: usize, b: usize, types: &mut [Type]) -> Result<(), String> {
			match (types[a].clone(), types[b].clone()) {
				(Type::Int, Type::Int)
				| (Type::Bool, Type::Bool)
				| (Type::String, Type::String) => Ok(()),
				(Type::Group(c), Type::Group(d)) if c.len() == d.len() => {
					c.into_iter().zip(d).try_fold((), |_, (e, f)| eq(e, f, types))
				}
				(Type::Block(c), Type::Block(d))
					if c.inputs.len() == d.inputs.len()
						&& c.outputs.len() == d.outputs.len() =>
				{
					c.inputs
						.into_iter()
						.zip(d.inputs)
						.try_fold((), |_, (e, f)| eq(e, f, types))?;
					c.outputs
						.into_iter()
						.zip(d.outputs)
						.try_fold((), |_, (e, f)| eq(e, f, types))?;
					Ok(())
				}
				(Type::Option(c), Type::Option(d)) => eq(c, d, types),
				(Type::Unknown, c) => {
					types[a] = c;
					Ok(())
				}
				(c, Type::Unknown) => {
					types[b] = c;
					Ok(())
				}
				(a, b) => Err(format!("types aren't equal: {:?}, {:?}", a, b)),
			}
		}
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		self.outputs
			.drain(i..)
			.zip(&mut other.inputs[j..])
			.try_fold((), |_, (a, b)| eq(a, *b, types))?;
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
type Context = Vec<HashMap<String, usize>>;
pub fn analyze(parse_trees: &[ParseTree]) -> Result<(Vec<Tree>, Vec<Type>), Error> {
	fn analyze(
		parse_trees: &[ParseTree],
		io: &mut Io,
		context: &mut Context,
		types: &mut Vec<Type>,
	) -> Result<Vec<Tree>, Error> {
		fn add_type(t: Type, types: &mut Vec<Type>) -> usize {
			types.push(t);
			types.len() - 1
		}
		fn clone_vars(
			i: usize,
			types: &mut Vec<Type>,
			vars: &mut HashMap<usize, usize>,
		) -> usize {
			match types[i].clone() {
				Type::Int | Type::Bool | Type::String => i,
				Type::Group(ts) => add_type(
					Type::Group(ts.into_iter().map(|t| clone_vars(t, types, vars)).collect()),
					types,
				),
				Type::Block(io) => add_type(
					Type::Block(Io::new(
						io.inputs.into_iter().map(|t| clone_vars(t, types, vars)).collect(),
						io.outputs.into_iter().map(|t| clone_vars(t, types, vars)).collect(),
					)),
					types,
				),
				Type::Option(t) => add_type(Type::Option(clone_vars(t, types, vars)), types),
				Type::Unknown => match vars.get(&i) {
					Some(j) => *j,
					None => {
						let v = add_type(Type::Unknown, types);
						vars.insert(i, v);
						v
					}
				},
			}
		}
		let mut out = vec![];
		for tree in parse_trees {
			let l = tree.location;
			let children;
			let mut t;
			match &tree.data {
				Parsed::Name(i) if i == "=" => match tree.children.as_slice() {
					s @ [a @ ParseTree { data: Parsed::Name(key), children: cs, .. }, _]
						if cs.is_empty() =>
					{
						let a = Tree::new(a, Io::new(vec![], vec![]), vec![]);
						let b = analyze(&s[1..], io, context, types)?.remove(0);
						children = vec![a, b];
						let v = add_type(Type::Unknown, types);
						t = Io::new(vec![v], vec![]);
						let frame =
							match context.iter_mut().rev().find(|frame| frame.contains_key(key)) {
								Some(frame) => frame,
								None => context.last_mut().unwrap(),
							};
						frame.insert(key.to_owned(), v);
					}
					s => {
						return Err(Error(format!("expected name and value, found: {:?}", s), l))
					}
				},
				Parsed::Name(i) if i == "@" => {
					let mut a = Io::new(vec![], vec![]);
					let mut xs = analyze(&tree.children[1..], &mut a, context, types)?;
					let x = analyze(&tree.children[0..1], &mut a, context, types)?.remove(0);
					xs.insert(0, x);
					children = xs;
					t = a;
				}
				Parsed::Name(i) => {
					children = analyze(&tree.children, io, context, types)?;
					t = match i.as_str() {
						"true" | "false" => Io::new(vec![], vec![1]),
						"add" | "sub" | "mul" => Io::new(vec![0, 0], vec![0]),
						"eq" => {
							let v = add_type(Type::Unknown, types);
							Io::new(vec![v, v], vec![1])
						}
						"lt" | "gt" => Io::new(vec![0, 0], vec![1]),
						"call" => match types[io.last(l)?].clone() {
							Type::Block(Io { inputs, outputs }) => {
								let mut i = inputs.clone();
								i.push(add_type(
									Type::Block(Io::new(inputs.clone(), outputs.clone())),
									types,
								));
								Io::new(i, outputs.clone())
							}
							t => Err(Error(format!("expected block, found {:?}", t), l))?,
						},
						"_if_" => {
							let v = add_type(Type::Unknown, types);
							Io::new(vec![1, v], vec![add_type(Type::Option(v), types)])
						}
						"_else_" => {
							let v = add_type(Type::Unknown, types);
							Io::new(vec![add_type(Type::Option(v), types), v], vec![v])
						}
						"_while_" => Io::new(
							vec![
								add_type(Type::Block(Io::new(vec![], vec![1])), types),
								add_type(Type::Block(Io::new(vec![], vec![])), types),
							],
							vec![],
						),
						"print" => Io::new(vec![add_type(Type::Unknown, types)], vec![]),
						key => {
							let a = *context
								.iter()
								.rev()
								.find_map(|frame| frame.get(key))
								.ok_or_else(|| Error(format!("\"{}\" not found", key), l))?;
							match types[a].clone() {
								Type::Block(io) => {
									let mut vars = HashMap::new();
									Io::new(
										io.inputs
											.into_iter()
											.map(|t| clone_vars(t, types, &mut vars))
											.collect(),
										io.outputs
											.into_iter()
											.map(|t| clone_vars(t, types, &mut vars))
											.collect(),
									)
								}
								_ => Io::new(vec![], vec![a]),
							}
						}
					};
				}
				Parsed::Number(_) => {
					children = analyze(&tree.children, io, context, types)?;
					t = Io::new(vec![], vec![0]);
				}
				Parsed::String(_) => {
					children = analyze(&tree.children, io, context, types)?;
					t = Io::new(vec![], vec![2]);
				}
				Parsed::Brackets(b) => {
					let mut a = Io::new(vec![], vec![]);
					children = analyze(&tree.children, &mut a, context, types)?;
					t = match b {
						Bracket::Round => a,
						Bracket::Curly => Io::new(vec![], vec![add_type(Type::Block(a), types)]),
						Bracket::Square => {
							Io::new(a.inputs, vec![add_type(Type::Group(a.outputs), types)])
						}
					};
				}
			}
			io.combine(&mut t, types).map_err(|s| Error(s, l))?;
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
	let mut types = vec![Type::Int, Type::Bool, Type::String];
	let mut trees = vec![];
	trees.extend(analyze(&parse(&tokenize(&prelude)?)?, &mut io, &mut context, &mut types)?);
	trees.append(&mut analyze(parse_trees, &mut io, &mut context, &mut types)?);
	if io.inputs.is_empty() {
		Ok((trees, types))
	} else {
		Err(Error(
			format!(
				"program expected {:?}",
				io.inputs.iter().map(|i| &types[*i]).collect::<Vec<&Type>>()
			),
			Location(0, 0),
		))
	}
}

#[test]
fn analyze_test() {
	let mut types = vec![Type::Int, Type::Bool, Type::String];
	let mut io = Io::new(vec![], vec![1]);
	io.combine(&mut Io::new(vec![0, 1], vec![0]), &mut types).unwrap();
	assert_eq!(io, Io::new(vec![0], vec![0]));

	let f = |s: &str| analyze(&parse(&tokenize(&s.chars().collect::<Vec<char>>())?)?);
	assert_eq!(f("1 2 add").unwrap().0.last().unwrap().io, Io::new(vec![0, 0], vec![0]));
	assert_eq!(
		f("true 2 add"),
		Err(Error("types aren't equal: Bool, Int".to_owned(), Location(7, 3)))
	);
	assert_eq!(f("2 add"), Err(Error("program expected [Int]".to_owned(), Location(0, 0))));
	assert_eq!(f("nop").unwrap().0[0].io.outputs, vec![]);
	let v = f("1 dup").unwrap().0.last().unwrap().io.inputs[0];
	assert_eq!(f("1 dup").unwrap().0.last().unwrap().io.outputs, vec![v, v]);
}
