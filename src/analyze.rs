use crate::parse::{parse, Parsed, Tree as ParseTree};
use crate::tokenize::{tokenize, Bracket};
use crate::{Error, Location, PRELUDE_PATH};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
	Int,
	Bool,
	String,
	Group(Vec<Type>),
	Block(Vec<Type>, Vec<Type>),
	Option(Box<Type>),
}

fn eq(a: &Type, b: &Type) -> Result<(), String> {
	match (a, b) {
		(Type::Int, Type::Int)
		| (Type::Bool, Type::Bool)
		| (Type::String, Type::String)
		| (Type::Block(..), Type::Block(..)) => Ok(()),
		(Type::Group(a), Type::Group(b)) => {
			a.iter().zip(b).try_fold((), |_, (a, b)| eq(a, b))
		}
		(Type::Option(a), Type::Option(b)) => eq(a, b),
		(a, b) => Err(format!("types aren't equal: {:?}, {:?}", a, b)),
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tree {
	pub inputs: Vec<Type>,
	pub outputs: Vec<Type>,
	pub data: Parsed,
	pub location: Location,
	pub children: Vec<Tree>,
}

impl Tree {
	fn new(
		tree: &ParseTree,
		inputs: Vec<Type>,
		outputs: Vec<Type>,
		children: Vec<Tree>,
	) -> Tree {
		Tree { inputs, outputs, data: tree.data.clone(), location: tree.location, children }
	}
}

use std::collections::HashMap;
type Context = Vec<HashMap<String, (Vec<Type>, Vec<Type>)>>;
pub fn analyze(trees: &[ParseTree]) -> Result<Vec<Tree>, Error> {
	fn analyze(
		trees: &[ParseTree],
		stack: &mut Vec<Type>,
		context: &mut Context,
	) -> Result<Vec<Tree>, Error> {
		fn last(stack: &mut [Type], l: Location) -> Result<&Type, Error> {
			stack.last().ok_or_else(|| Error("no type on stack".to_owned(), l))
		}
		fn pop(stack: &mut Vec<Type>, l: Location) -> Result<Type, Error> {
			stack.pop().ok_or_else(|| Error("no type on stack".to_owned(), l))
		}
		let mut out = vec![];
		for tree in trees {
			let l = tree.location;
			out.push(match &tree.data {
				Parsed::Name(i) if i == "=" => {
					let key = match tree.children.get(0) {
						Some(ParseTree { data: Parsed::Name(key), .. }) => key,
						Some(_) => Err(Error("invalid var name".to_owned(), l))?,
						None => Err(Error("no var name".to_owned(), l))?,
					};
					let mut rest = analyze(&tree.children[1..], stack, context)?;
					let io = (vec![pop(stack, l)?], vec![]);
					match context.iter_mut().find(|frame| frame.contains_key(key)) {
						Some(frame) => frame,
						None => context.last_mut().unwrap(),
					}
					.insert(key.to_owned(), io.clone());
					rest.insert(0, Tree::new(&tree.children[0], vec![], vec![], vec![]));
					Tree::new(tree, io.0, io.1, rest)
				}
				Parsed::Name(i) => {
					let children = analyze(&tree.children, stack, context)?;
					let (inputs, outputs) = match i.as_str() {
						"@" => match stack.as_slice() {
							[.., Type::Block(i, o), _] if i.len() == 1 => (
								vec![Type::Block(i.clone(), o.clone()), i[0].clone()],
								o.clone(),
							),
							[.., f, x] => {
								Err(Error(format!("invalid @ types: {:?}, {:?}", f, x), l))?
							}
							_ => Err(Error("no type on stack".to_owned(), l))?,
						},
						"true" | "false" => (vec![], vec![Type::Bool]),
						"add" | "sub" | "mul" => (vec![Type::Int, Type::Int], vec![Type::Int]),
						"not" => (vec![Type::Bool], vec![Type::Bool]),
						"eq" => (
							vec![last(stack, l)?.clone(), last(stack, l)?.clone()],
							vec![Type::Bool],
						),
						"lt" | "gt" => (vec![Type::Int, Type::Int], vec![Type::Bool]),
						"call" => match last(stack, l)? {
							Type::Block(i, o) => {
								let mut io = (i.clone(), o.clone());
								io.0.push(Type::Block(i.clone(), o.clone()));
								io
							}
							t => Err(Error(format!("expected block, found {:?}", t), l))?,
						},
						"swap" => match stack.as_slice() {
							[.., a, b] => {
								(vec![a.clone(), b.clone()], vec![b.clone(), a.clone()])
							}
							_ => Err(Error("no type on stack".to_owned(), l))?,
						},
						"_if_" => (
							vec![Type::Bool, last(stack, l)?.clone()],
							vec![Type::Option(Box::new(last(stack, l)?.clone()))],
						),
						"_else_" => (
							vec![
								Type::Option(Box::new(last(stack, l)?.clone())),
								last(stack, l)?.clone(),
							],
							vec![last(stack, l)?.clone()],
						),
						"_while_" => (
							vec![
								Type::Block(vec![], vec![Type::Bool]),
								Type::Block(vec![], vec![]),
							],
							vec![],
						),
						"print" => (vec![last(stack, l)?.clone()], vec![]),
						key => {
							let t = context
								.iter()
								.find_map(|frame| frame.get(key))
								.ok_or_else(|| Error(format!("var {} not found", key), l))?;
							match t.1.get(0) {
								Some(Type::Block(i, o)) if t.0.is_empty() && t.1.len() == 1 => {
									(i.clone(), o.clone())
								}
								_ => t.clone(),
							}
						}
					};
					for (a, b) in inputs.iter().zip(stack.drain(stack.len() - inputs.len()..)) {
						eq(a, &b).map_err(|s| Error(s, l))?
					}
					stack.extend(outputs.clone());
					Tree::new(tree, inputs, outputs, children)
				}
				Parsed::String(_) => {
					stack.push(Type::String);
					Tree::new(tree, vec![], vec![Type::String], vec![])
				}
				Parsed::Number(_) => {
					stack.push(Type::Int);
					Tree::new(tree, vec![], vec![Type::Int], vec![])
				}
				Parsed::Brackets(Bracket::Round) => {
					let mut s = vec![];
					let children = analyze(&tree.children, &mut s, context)?;
					stack.extend(s.clone());
					Tree::new(tree, vec![], s, children)
				}
				Parsed::Brackets(Bracket::Curly) => {
					let mut s = stack.clone();
					let mut c = context.clone();
					c.push(HashMap::new());
					let _children = analyze(&tree.children, &mut s, &mut c)?;
					c.pop();
					todo!("accumulate bracketed types and block");
				}
				Parsed::Brackets(Bracket::Square) => {
					let mut s = vec![];
					let children = analyze(&tree.children, &mut s, context)?;
					stack.push(Type::Group(s.clone()));
					Tree::new(tree, vec![], vec![Type::Group(s)], children)
				}
			});
		}
		Ok(out)
	}
	let prelude = std::fs::read_to_string(PRELUDE_PATH)
		.map_err(|_| Error("could not read prelude".to_owned(), Location(0, 0)))?
		.chars()
		.collect::<Vec<char>>();
	let mut stack = vec![];
	let mut context = vec![HashMap::new()];
	analyze(&parse(&tokenize(&prelude)?)?, &mut stack, &mut context)?;
	analyze(trees, &mut stack, &mut context)
}

#[test]
fn analyze_test() {
	let f = |s: &str| analyze(&parse(&tokenize(&s.chars().collect::<Vec<char>>())?)?);
	assert!(f("1 2 add").is_ok());
}
