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

	fn combine(&mut self, other: &mut Io, types: &mut Types) -> Result<(), String> {
		fn eq(a: usize, b: usize, types: &mut Types) -> Result<(), String> {
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
					types.0[a] = c;
					Ok(())
				}
				(c, Type::Unknown) => {
					types.0[b] = c;
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

#[derive(Debug)]
struct Types(Vec<Type>);

impl Types {
	fn new() -> Types {
		Types(vec![Type::Int, Type::Bool, Type::String])
	}

	fn add(&mut self, t: Type) -> usize {
		for (i, s) in self.0.iter().enumerate() {
			if t != Type::Unknown && *s == t {
				return i;
			}
		}
		self.0.push(t);
		self.0.len() - 1
	}
}

impl std::ops::Index<usize> for Types {
	type Output = Type;
	fn index(&self, i: usize) -> &Type {
		&self.0[i]
	}
}

use std::collections::HashMap;
type Context = Vec<HashMap<String, usize>>;
pub fn analyze(parse_trees: &[ParseTree]) -> Result<(Vec<Tree>, Vec<Type>), Error> {
	fn analyze(
		parse_trees: &[ParseTree],
		io: &mut Io,
		context: &mut Context,
		types: &mut Types,
	) -> Result<Vec<Tree>, Error> {
		fn clone_vars(i: usize, types: &mut Types, vars: &mut HashMap<usize, usize>) -> usize {
			match types[i].clone() {
				Type::Int | Type::Bool | Type::String => i,
				Type::Group(ts) => {
					let t = Type::Group(
						ts.into_iter().map(|t| clone_vars(t, types, vars)).collect(),
					);
					types.add(t)
				}
				Type::Block(io) => {
					let t = Type::Block(Io::new(
						io.inputs.into_iter().map(|t| clone_vars(t, types, vars)).collect(),
						io.outputs.into_iter().map(|t| clone_vars(t, types, vars)).collect(),
					));
					types.add(t)
				}
				Type::Option(t) => {
					let t = Type::Option(clone_vars(t, types, vars));
					types.add(t)
				}
				Type::Unknown => match vars.get(&i) {
					Some(j) => *j,
					None => {
						let v = types.add(Type::Unknown);
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
						let a = Tree {
							io: Io::new(vec![], vec![]),
							data: a.data.clone(),
							location: l,
							children: vec![],
						};
						let b = analyze(&s[1..], io, context, types)?.remove(0);
						children = vec![a, b];
						let v = types.add(Type::Unknown);
						t = Io::new(vec![v], vec![]);
						let frame =
							match context.iter_mut().rev().find(|frame| frame.contains_key(key))
							{
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
							let v = types.add(Type::Unknown);
							Io::new(vec![v, v], vec![1])
						}
						"lt" | "gt" => Io::new(vec![0, 0], vec![1]),
						"call" => {
							match types[*io.outputs.last().unwrap()].clone() {
								Type::Block(Io { inputs, outputs }) => {
									let mut i = inputs.clone();
									i.push(types.add(Type::Block(Io::new(
										inputs.clone(),
										outputs.clone(),
									))));
									Io::new(i, outputs.clone())
								}
								t => Err(Error(format!("expected block, found {:?}", t), l))?,
							}
						}
						"_if_" => {
							let v = types.add(Type::Unknown);
							Io::new(vec![1, v], vec![types.add(Type::Option(v))])
						}
						"_else_" => {
							let v = types.add(Type::Unknown);
							Io::new(vec![types.add(Type::Option(v)), v], vec![v])
						}
						"_while_" => Io::new(
							vec![
								types.add(Type::Block(Io::new(vec![], vec![1]))),
								types.add(Type::Block(Io::new(vec![], vec![]))),
							],
							vec![],
						),
						"print" => Io::new(vec![types.add(Type::Unknown)], vec![]),
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
						Bracket::Curly => Io::new(vec![], vec![types.add(Type::Block(a))]),
						Bracket::Square => {
							Io::new(a.inputs, vec![types.add(Type::Group(a.outputs))])
						}
					};
				}
			}
			io.combine(&mut t, types).map_err(|s| Error(s, l))?;
			out.push(Tree { io: t, data: tree.data.clone(), location: l, children });
		}
		Ok(out)
	}
	let prelude = std::fs::read_to_string(PRELUDE_PATH)
		.map_err(|_| Error("could not read prelude".to_owned(), Location(0, 0)))?
		.chars()
		.collect::<Vec<char>>();
	let mut io = Io::new(vec![], vec![]);
	let mut context = vec![HashMap::new()];
	let mut types = Types::new();
	let mut trees = vec![];
	trees.extend(analyze(&parse(&tokenize(&prelude)?)?, &mut io, &mut context, &mut types)?);
	trees.append(&mut analyze(parse_trees, &mut io, &mut context, &mut types)?);
	if io.inputs.is_empty() {
		Ok((trees, types.0))
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
	let mut io = Io::new(vec![], vec![1]);
	io.combine(&mut Io::new(vec![0, 1], vec![0]), &mut Types::new()).unwrap();
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
