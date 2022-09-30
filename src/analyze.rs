use crate::parse::{parse, Parsed, Tree as ParseTree};
use crate::tokenize::{tokenize, Bracket};
use crate::{Error, Location, PRELUDE_PATH};
use std::collections::HashMap;
type Context = crate::context::C<String, usize>;

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

	fn combine(&mut self, other: &Io, types: &mut Types) -> Result<(), String> {
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		self.outputs
			.drain(i..)
			.zip(&other.inputs[j..])
			.try_fold((), |_, (a, b)| types.eq(a, *b))?;
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
		self.0.push(t);
		self.0.len() - 1
	}

	fn clone_vars(&mut self, i: usize, vars: &mut HashMap<usize, usize>) -> usize {
		match self.0[i].clone() {
			Type::Int | Type::Bool | Type::String => i,
			Type::Group(ts) => {
				let t = Type::Group(ts.into_iter().map(|t| self.clone_vars(t, vars)).collect());
				self.add(t)
			}
			Type::Block(io) => {
				let t = Type::Block(Io::new(
					io.inputs.into_iter().map(|t| self.clone_vars(t, vars)).collect(),
					io.outputs.into_iter().map(|t| self.clone_vars(t, vars)).collect(),
				));
				self.add(t)
			}
			Type::Option(t) => {
				let t = Type::Option(self.clone_vars(t, vars));
				self.add(t)
			}
			Type::Unknown => match vars.get(&i) {
				Some(j) => *j,
				None => {
					let v = self.add(Type::Unknown);
					vars.insert(i, v);
					v
				}
			},
		}
	}

	fn eq(&mut self, a: usize, b: usize) -> Result<(), String> {
		match (self.0[a].clone(), self.0[b].clone()) {
			(Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::String, Type::String) => {
				Ok(())
			}
			(Type::Group(c), Type::Group(d)) if c.len() == d.len() => {
				c.into_iter().zip(d).try_fold((), |_, (e, f)| self.eq(e, f))
			}
			(Type::Block(c), Type::Block(d))
				if c.inputs.len() == d.inputs.len() && c.outputs.len() == d.outputs.len() =>
			{
				c.inputs.into_iter().zip(d.inputs).try_fold((), |_, (e, f)| self.eq(e, f))?;
				c.outputs.into_iter().zip(d.outputs).try_fold((), |_, (e, f)| self.eq(e, f))?;
				Ok(())
			}
			(Type::Option(c), Type::Option(d)) => self.eq(c, d),
			(Type::Unknown, c) => {
				self.0[a] = c;
				Ok(())
			}
			(c, Type::Unknown) => {
				self.0[b] = c;
				Ok(())
			}
			(a, b) => Err(format!("types aren't equal: {:?}, {:?}", a, b)),
		}
	}
}

pub fn analyze(parse_trees: &[ParseTree]) -> Result<(Vec<Tree>, Vec<Type>), Error> {
	fn analyze(
		parse_trees: &[ParseTree],
		io: &mut Io,
		context: Context,
		types: &mut Types,
	) -> Result<Vec<Tree>, Error> {
		let mut out = vec![];
		for tree in parse_trees {
			let l = tree.location;
			let (mut t, mut children);
			match &tree.data {
				Parsed::Name(i) if i == "=" => {
					let key = match tree.children.get(0) {
						Some(ParseTree { data: Parsed::Name(key), .. }) => key.clone(),
						Some(_) => Err(Error("invalid var name".to_owned(), l))?,
						None => Err(Error("no var name".to_owned(), l))?,
					};

					children = analyze(&tree.children[1..], io, context.clone(), types)?;
					children.insert(
						0,
						Tree {
							io: Io::new(vec![], vec![]),
							data: Parsed::Name(key.clone()),
							location: l,
							children: vec![],
						},
					);

					let v = types.add(Type::Unknown);
					t = Io::new(vec![v], vec![]);
					if let Some(w) = context.set(key, v) {
						types.eq(v, w).map_err(|s| Error(s, l))?;
					}
				}
				Parsed::Name(i) if i == "@" => {
					t = Io::new(vec![], vec![]);
					children = analyze(&tree.children[1..], &mut t, context.clone(), types)?;
					children.splice(
						0..0,
						analyze(&tree.children[0..1], &mut t, context.clone(), types)?,
					);
				}
				Parsed::Name(i) => {
					children = analyze(&tree.children, io, context.clone(), types)?;
					t =
						match i.as_str() {
							"true" | "false" => Io::new(vec![], vec![1]),
							"add" | "sub" | "mul" => Io::new(vec![0, 0], vec![0]),
							"eq" => {
								let v = types.add(Type::Unknown);
								Io::new(vec![v, v], vec![1])
							}
							"lt" | "gt" => Io::new(vec![0, 0], vec![1]),
							"call" => match types.0[*io.outputs.last().unwrap()].clone() {
								Type::Block(Io { inputs, outputs }) => {
									let mut i = inputs.clone();
									i.push(types.add(Type::Block(Io::new(
										inputs.clone(),
										outputs.clone(),
									))));
									Io::new(i, outputs.clone())
								}
								t => Err(Error(format!("expected block, found {:?}", t), l))?,
							},
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
								let a = context
									.get(key)
									.ok_or_else(|| Error(format!("{} not found", key), l))?;
								match types.0[a].clone() {
									Type::Block(io) => {
										let mut vars = HashMap::new();
										Io::new(
											io.inputs
												.into_iter()
												.map(|t| types.clone_vars(t, &mut vars))
												.collect(),
											io.outputs
												.into_iter()
												.map(|t| types.clone_vars(t, &mut vars))
												.collect(),
										)
									}
									_ => Io::new(vec![], vec![a]),
								}
							}
						};
				}
				Parsed::Number(_) => {
					children = analyze(&tree.children, io, context.clone(), types)?;
					t = Io::new(vec![], vec![0]);
				}
				Parsed::String(_) => {
					children = analyze(&tree.children, io, context.clone(), types)?;
					t = Io::new(vec![], vec![2]);
				}
				Parsed::Brackets(Bracket::Round) => {
					t = Io::new(vec![], vec![]);
					children = analyze(&tree.children, &mut t, context.clone(), types)?;
				}
				Parsed::Brackets(Bracket::Curly) => {
					let mut a = Io::new(vec![], vec![]);
					children = analyze(
						&tree.children,
						&mut a,
						Context::new_from(context.clone()),
						types,
					)?;
					t = Io::new(vec![], vec![types.add(Type::Block(a))]);
				}
				Parsed::Brackets(Bracket::Square) => {
					let mut a = Io::new(vec![], vec![]);
					children = analyze(&tree.children, &mut a, context.clone(), types)?;
					t = Io::new(a.inputs, vec![types.add(Type::Group(a.outputs))]);
				}
			}
			io.combine(&t, types).map_err(|s| Error(s, l))?;
			out.push(Tree { io: t, data: tree.data.clone(), location: l, children });
		}
		Ok(out)
	}
	let prelude = std::fs::read_to_string(PRELUDE_PATH)
		.map_err(|_| Error("could not read prelude".to_owned(), Location(0, 0)))?
		.chars()
		.collect::<Vec<char>>();
	let mut io = Io::new(vec![], vec![]);
	let context = Context::new();
	let mut types = Types::new();
	let mut trees = vec![];
	trees.extend(analyze(&parse(&tokenize(&prelude)?)?, &mut io, context.clone(), &mut types)?);
	trees.append(&mut analyze(parse_trees, &mut io, context, &mut types)?);
	if io.inputs.is_empty() {
		Ok((trees, types.0))
	} else {
		Err(Error(
			format!(
				"program expected {:?}",
				io.inputs.iter().map(|i| &types.0[*i]).collect::<Vec<&Type>>()
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
	let (trees, types) = f("b = 1 {not false} b").unwrap();
	assert_eq!(types[trees.last().unwrap().io.outputs[0]], Type::Int);
}
