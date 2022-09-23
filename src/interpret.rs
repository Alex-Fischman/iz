use crate::parse::{parse, Parsed, Tree};
use crate::tokenize::{tokenize, Bracket};
use crate::{Error, Location, PRELUDE_PATH};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Int(i64),
	Bool(bool),
	String(String),
	Group(Vec<Value>),
	Block(Vec<Tree>),
	Some(Box<Value>),
	None,
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Value::Int(i) => write!(f, "{}", i),
			Value::Bool(b) => write!(f, "{}", b),
			Value::String(s) => write!(f, "{}", s),
			Value::Group(v) => {
				write!(f, "[ ")?;
				for v in v {
					write!(f, "{} ", v)?;
				}
				write!(f, "]")
			}
			Value::Block(_) => write!(f, "Block"),
			Value::Some(v) => write!(f, "Some({})", v),
			Value::None => write!(f, "None"),
		}
	}
}

use std::collections::HashMap;
type Context = Vec<HashMap<String, Value>>;
pub fn interpret(trees: &[Tree]) -> Result<Vec<Value>, Error> {
	fn interpret(
		trees: &[Tree],
		stack: &mut Vec<Value>,
		context: &mut Context,
	) -> Result<(), Error> {
		fn pop(stack: &mut Vec<Value>, l: Location) -> Result<Value, Error> {
			stack.pop().ok_or_else(|| Error("no value on stack".to_owned(), l))
		}
		fn eq(a: &Value, b: &Value, l: Location) -> Result<bool, Error> {
			match (a, b) {
				(Value::Int(a), Value::Int(b)) => Ok(a == b),
				(Value::Bool(a), Value::Bool(b)) => Ok(a == b),
				(Value::String(a), Value::String(b)) => Ok(a == b),
				(Value::Group(a), Value::Group(b)) => {
					a.iter().zip(b).try_fold(true, |acc, (a, b)| Ok(acc && eq(a, b, l)?))
				}
				_ => Err(Error("invalid _eq_ args".to_owned(), l))?,
			}
		}
		fn call(
			stack: &mut Vec<Value>,
			context: &mut Context,
			l: Location,
		) -> Result<(), Error> {
			match pop(stack, l)? {
				Value::Block(v) => {
					context.push(HashMap::new());
					let out = interpret(&v, stack, context);
					context.pop();
					out
				}
				_ => Err(Error("invalid _call_ args".to_owned(), l))?,
			}
		}
		fn swap(stack: &mut Vec<Value>, l: Location) -> Result<(), Error> {
			let (b, a) = (pop(stack, l)?, pop(stack, l)?);
			stack.push(b);
			stack.push(a);
			Ok(())
		}
		for tree in trees {
			let l = tree.location;
			match &tree.data {
				Parsed::Name(i) if i == "=" => {
					let key = match tree.children.get(0) {
						Some(Tree { data: Parsed::Name(key), .. }) => key,
						Some(_) => Err(Error("invalid var name".to_owned(), l))?,
						None => Err(Error("no var name".to_owned(), l))?,
					};
					interpret(&tree.children[1..], stack, context)?;
					match context.iter_mut().find(|frame| frame.contains_key(key)) {
						Some(frame) => frame,
						None => context.last_mut().unwrap(),
					}
					.insert(key.to_owned(), pop(stack, l)?);
				}
				Parsed::Name(i) if i == "@" => {
					interpret(&tree.children[1..], stack, context)?;
					interpret(&tree.children[0..1], stack, context)?;
					if let Some(Value::Block(_)) = stack.last() {
						call(stack, context, l)?;
					}
				}
				Parsed::Name(i) => {
					interpret(&tree.children, stack, context)?;
					match i.as_str() {
						"true" => stack.push(Value::Bool(true)),
						"false" => stack.push(Value::Bool(false)),
						"add" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a + b)),
							_ => Err(Error("invalid _add_ args".to_owned(), l))?,
						},
						"sub" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a - b)),
							_ => Err(Error("invalid _sub_ args".to_owned(), l))?,
						},
						"mul" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a * b)),
							_ => Err(Error("invalid _mul_ args".to_owned(), l))?,
						},
						"not" => match pop(stack, l)? {
							Value::Bool(b) => stack.push(Value::Bool(!b)),
							_ => Err(Error("invalid _not_ args".to_owned(), l))?,
						},
						"eq" => {
							let (b, a) = (pop(stack, l)?, pop(stack, l)?);
							stack.push(Value::Bool(eq(&a, &b, l)?));
						}
						"lt" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a < b)),
							_ => Err(Error("invalid _lt_ args".to_owned(), l))?,
						},
						"gt" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a > b)),
							_ => Err(Error("invalid _gt_ args".to_owned(), l))?,
						},
						"call" => call(stack, context, l)?,
						"swap" => swap(stack, l)?,
						"_if_" => {
							swap(stack, l)?;
							match pop(stack, l)? {
								Value::Bool(true) => {
									call(stack, context, l)?;
									let c = pop(stack, l)?;
									stack.push(Value::Some(Box::new(c)))
								}
								Value::Bool(false) => stack.push(Value::None),
								_ => Err(Error("invalid _if_ args".to_owned(), l))?,
							}
						}
						"_else_" => match (pop(stack, l)?, pop(stack, l)?) {
							(_, Value::Some(a)) => stack.push(*a),
							(b, Value::None) => stack.push(b),
							_ => Err(Error("invalid _else_ args".to_owned(), l))?,
						},
						"_while_" => {
							let (b, a) = (pop(stack, l)?, pop(stack, l)?);
							while {
								stack.push(a.clone());
								call(stack, context, l)?;
								match pop(stack, l)? {
									Value::Bool(b) => b,
									_ => Err(Error("invalid _while_ args".to_owned(), l))?,
								}
							} {
								stack.push(b.clone());
								call(stack, context, l)?;
							}
						}
						"print" => print!("{}", pop(stack, l)?),
						key => {
							stack.push(
								context
									.iter()
									.find_map(|frame| frame.get(key))
									.ok_or_else(|| Error("var not found".to_owned(), l))?
									.clone(),
							);
							if let Some(Value::Block(_)) = stack.last() {
								call(stack, context, l)?;
							}
						}
					}
				}
				Parsed::String(s) => stack.push(Value::String(s.clone())),
				Parsed::Number(n) => stack.push(Value::Int(*n)),
				Parsed::Brackets(Bracket::Round) => interpret(&tree.children, stack, context)?,
				Parsed::Brackets(Bracket::Curly) => {
					stack.push(Value::Block(tree.children.clone()))
				}
				Parsed::Brackets(Bracket::Square) => {
					let mut s = vec![];
					interpret(&tree.children, &mut s, context)?;
					stack.push(Value::Group(s));
				}
			}
		}
		Ok(())
	}
	let prelude = std::fs::read_to_string(PRELUDE_PATH)
		.map_err(|_| Error("could not read prelude".to_owned(), Location(0, 0)))?
		.chars()
		.collect::<Vec<char>>();
	let mut stack = vec![];
	let mut context = vec![HashMap::new()];
	interpret(&parse(&tokenize(&prelude)?)?, &mut stack, &mut context)?;
	interpret(trees, &mut stack, &mut context)?;
	Ok(stack)
}

#[test]
fn interpret_test() {
	let f = |s: &str| interpret(&parse(&tokenize(&s.chars().collect::<Vec<char>>())?)?);
	assert_eq!(f("true 1 sub"), Err(Error("invalid _sub_ args".to_owned(), Location(7, 3))));
	assert_eq!(f("1 sub"), Err(Error("no value on stack".to_owned(), Location(2, 3))));
	assert_eq!(f("1 - 2"), Ok(vec![Value::Int(-1)]));
	assert_eq!(f("1 - 2 - 3"), Ok(vec![Value::Int(-4)]));
	assert_eq!(f("1 != 2"), Ok(vec![Value::Bool(true)]));
	assert_eq!(f("1 > 2"), Ok(vec![Value::Bool(false)]));
	assert_eq!(
		f("1 2 [3 4] 5 6"),
		Ok(vec![
			Value::Int(1),
			Value::Int(2),
			Value::Group(vec![Value::Int(3), Value::Int(4)]),
			Value::Int(5),
			Value::Int(6)
		])
	);
	assert_eq!(f("{2 mul}@3"), Ok(vec![Value::Int(6)]));
	assert_eq!(f("{2 * 3} call"), Ok(vec![Value::Int(6)]));
	assert_eq!(f("if true {1}"), Ok(vec![Value::Some(Box::new(Value::Int(1)))]));
	assert_eq!(f("if true {1} else {2}"), Ok(vec![Value::Int(1)]));
	assert_eq!(f("i = 1 + 2 i"), Ok(vec![Value::Int(3)]));
	assert_eq!(f("{i = 1 + 2} call i"), Err(Error("var not found".to_owned(), Location(17, 1))));
	assert_eq!(f("!true"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("not@true"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("1 dup"), Ok(vec![Value::Int(1), Value::Int(1)]));
}
