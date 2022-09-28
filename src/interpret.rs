use crate::analyze::{analyze, Tree, Type};
use crate::parse::{parse, Parsed};
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
				(Value::Block(_a), Value::Block(_b)) => Ok(false),
				(Value::Some(a), Value::Some(b)) => eq(a, b, l),
				(Value::Some(_), Value::None) | (Value::None, Value::Some(_)) => Ok(false),
				(Value::None, Value::None) => Ok(true),
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
					let frame = match context.iter_mut().find(|frame| frame.contains_key(key)) {
						Some(frame) => frame,
						None => context.last_mut().unwrap(),
					};
					frame.insert(key.to_owned(), pop(stack, l)?);
				}
				Parsed::Name(i) if i == "@" => {
					interpret(&tree.children[1..], stack, context)?;
					interpret(&tree.children[0..1], stack, context)?;
				}
				Parsed::Name(i) => {
					interpret(&tree.children, stack, context)?;
					use Type::*;
					match (i.as_str(), tree.io.inputs.as_slice(), tree.io.outputs.as_slice()) {
						("true", [], [Bool]) => stack.push(Value::Bool(true)),
						("false", [], [Bool]) => stack.push(Value::Bool(false)),
						("add", [Int, Int], [Int]) => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a + b)),
							_ => Err(Error("invalid _add_ args".to_owned(), l))?,
						},
						("sub", [Int, Int], [Int]) => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a - b)),
							_ => Err(Error("invalid _sub_ args".to_owned(), l))?,
						},
						("mul", [Int, Int], [Int]) => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a * b)),
							_ => Err(Error("invalid _mul_ args".to_owned(), l))?,
						},
						("not", [Bool], [Bool]) => match pop(stack, l)? {
							Value::Bool(b) => stack.push(Value::Bool(!b)),
							_ => Err(Error("invalid _not_ args".to_owned(), l))?,
						},
						("eq", [_, _], [Bool]) => {
							let (b, a) = (pop(stack, l)?, pop(stack, l)?);
							stack.push(Value::Bool(eq(&a, &b, l)?));
						}
						("lt", [Int, Int], [Bool]) => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a < b)),
							_ => Err(Error("invalid _lt_ args".to_owned(), l))?,
						},
						("gt", [Int, Int], [Bool]) => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a > b)),
							_ => Err(Error("invalid _gt_ args".to_owned(), l))?,
						},
						("call", _, _) => call(stack, context, l)?,
						("_if_", [Bool, a], [Option(b)]) if *a == **b => {
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
						("_else_", [Option(a), b], [c]) if **a == *b && b == c => {
							match (pop(stack, l)?, pop(stack, l)?) {
								(_, Value::Some(a)) => stack.push(*a),
								(b, Value::None) => stack.push(b),
								_ => Err(Error("invalid _else_ args".to_owned(), l))?,
							}
						}
						("_while_", [Block(a), Block(b)], [])
							if a.inputs.is_empty()
								&& a.outputs == vec![Bool] && b.inputs.is_empty()
								&& b.outputs.is_empty() =>
						{
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
						("print", _, []) => print!("{}", pop(stack, l)?),
						(key, _, _) => {
							stack.push(
								context
									.iter()
									.find_map(|frame| frame.get(key))
									.ok_or_else(|| Error(format!("{} not found", key), l))?
									.clone(),
							);
							if let Some(Value::Block(_)) = stack.last() {
								call(stack, context, l)?
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
	interpret(&analyze(&parse(&tokenize(&prelude)?)?)?, &mut stack, &mut context)?;
	interpret(trees, &mut stack, &mut context)?;
	Ok(stack)
}

#[test]
fn interpret_test() {
	let f =
		|s: &str| interpret(&analyze(&parse(&tokenize(&s.chars().collect::<Vec<char>>())?)?)?);
	assert_eq!(
		f("true 1 sub"),
		Err(Error("types aren't equal: Bool, Int".to_owned(), Location(7, 3)))
	);
	assert_eq!(f("1 sub"), Err(Error("program expected [Int]".to_owned(), Location(0, 0))));
	assert_eq!(f("1 - 2"), Ok(vec![Value::Int(-1)]));
	assert_eq!(f("1 2 sub"), Ok(vec![Value::Int(-1)]));
	assert_eq!(f("1 - 2 - 3"), Ok(vec![Value::Int(-4)]));
	assert_eq!(f("1 == 2"), Ok(vec![Value::Bool(false)]));
	// assert_eq!(f("1 != 2"), Ok(vec![Value::Bool(true)]));
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
	assert_eq!(f("(2 mul)@3"), Ok(vec![Value::Int(6)]));
	assert_eq!(f("{2 * 3} call"), Ok(vec![Value::Int(6)]));
	// assert_eq!(f("if true {1}"), Ok(vec![Value::Some(Box::new(Value::Int(1)))]));
	// assert_eq!(f("if true {1} else {2}"), Ok(vec![Value::Int(1)]));
	assert_eq!(f("i = 1 + 2 i"), Ok(vec![Value::Int(3)]));
	assert_eq!(f("{i = 1 + 2} call i"), Err(Error("i not found".to_owned(), Location(17, 1))));
	assert_eq!(f("!true"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("not@true"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("1 nop"), Ok(vec![Value::Int(1)]));
	// assert_eq!(f("1 dup"), Ok(vec![Value::Int(1), Value::Int(1)]));
	// assert_eq!(f("1 pop"), Ok(vec![]));
}
