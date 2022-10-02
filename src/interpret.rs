use crate::analyze::{Tree, Type};
use crate::parse::Parsed;
use crate::tokenize::Bracket;
use crate::{Error, Location};
type Context = crate::Context<String, Value>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Int(i64),
	Bool(bool),
	String(String),
	Block(Vec<Tree>, Context),
	Some(Box<Value>),
	None,
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Value::Int(i) => write!(f, "{}", i),
			Value::Bool(b) => write!(f, "{}", b),
			Value::String(s) => write!(f, "{}", s),
			Value::Block(..) => write!(f, "Block"),
			Value::Some(v) => write!(f, "Some({})", v),
			Value::None => write!(f, "None"),
		}
	}
}

pub fn interpret(trees: &[Tree], types: &[Type]) -> Result<Vec<Value>, Error> {
	fn interpret(
		trees: &[Tree],
		stack: &mut Vec<Value>,
		context: Context,
		types: &[Type],
	) -> Result<(), Error> {
		fn pop(stack: &mut Vec<Value>, l: Location) -> Result<Value, Error> {
			stack.pop().ok_or_else(|| Error("no value on stack".to_owned(), l))
		}
		fn eq(a: &Value, b: &Value, l: Location) -> Result<bool, Error> {
			match (a, b) {
				(Value::Int(a), Value::Int(b)) => Ok(a == b),
				(Value::Bool(a), Value::Bool(b)) => Ok(a == b),
				(Value::String(a), Value::String(b)) => Ok(a == b),
				(Value::Block(..), Value::Block(..)) => Ok(false),
				(Value::Some(a), Value::Some(b)) => eq(a, b, l),
				(Value::Some(_), Value::None) | (Value::None, Value::Some(_)) => Ok(false),
				(Value::None, Value::None) => Ok(true),
				_ => Err(Error("invalid _eq_ args".to_owned(), l))?,
			}
		}
		fn call(stack: &mut Vec<Value>, types: &[Type], l: Location) -> Result<(), Error> {
			match pop(stack, l)? {
				Value::Block(v, c) => interpret(&v, stack, c, types),
				_ => Err(Error("invalid _call_ args".to_owned(), l))?,
			}
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
					interpret(&tree.children[1..], stack, context.clone(), types)?;
					context.set(key.to_owned(), pop(stack, l)?);
				}
				Parsed::Name(i) if i == "@" => {
					interpret(&tree.children[1..], stack, context.clone(), types)?;
					interpret(&tree.children[0..1], stack, context.clone(), types)?;
				}
				Parsed::Name(i) => {
					interpret(&tree.children, stack, context.clone(), types)?;
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
						"_if_" => match (pop(stack, l)?, pop(stack, l)?) {
							(b, Value::Bool(true)) => stack.push(Value::Some(Box::new(b))),
							(_, Value::Bool(false)) => stack.push(Value::None),
							_ => Err(Error("invalid _if_ args".to_owned(), l))?,
						},
						"_else_" => match (pop(stack, l)?, pop(stack, l)?) {
							(_, Value::Some(a)) => stack.push(*a),
							(b, Value::None) => stack.push(b),
							_ => Err(Error("invalid _else_ args".to_owned(), l))?,
						},
						"_while_" => {
							let (b, a) = (pop(stack, l)?, pop(stack, l)?);
							while {
								stack.push(a.clone());
								call(stack, types, l)?;
								match pop(stack, l)? {
									Value::Bool(b) => b,
									_ => Err(Error("invalid _while_ args".to_owned(), l))?,
								}
							} {
								stack.push(b.clone());
								call(stack, types, l)?;
							}
						}
						"print" => print!("{}", pop(stack, l)?),
						key => {
							stack.push(
								context
									.get(key)
									.ok_or_else(|| Error(format!("{} not found", key), l))?,
							);
							if let Some(Value::Block(..)) = stack.last() {
								call(stack, types, l)?
							}
						}
					}
				}
				Parsed::String(s) => stack.push(Value::String(s.clone())),
				Parsed::Number(n) => stack.push(Value::Int(*n)),
				Parsed::Brackets(Bracket::Round) => {
					interpret(&tree.children, stack, context.clone(), types)?
				}
				Parsed::Brackets(Bracket::Curly) => stack.push(Value::Block(
					tree.children.clone(),
					Context::new(Some(context.clone())),
				)),
				Parsed::Brackets(Bracket::Square) => todo!(),
			}
		}
		Ok(())
	}
	let mut stack = vec![];
	interpret(trees, &mut stack, Context::new(None), types)?;
	Ok(stack)
}

#[test]
fn interpret_test() {
	let f = |s: &str| {
		use crate::analyze::analyze;
		use crate::parse::parse;
		use crate::tokenize::tokenize;
		let (trees, types) = &analyze(&parse(&tokenize(&s.chars().collect::<Vec<char>>())?)?)?;
		interpret(trees, types)
	};
	assert_eq!(
		f("true 1 sub"),
		Err(Error("types aren't equal: Bool, Int".to_owned(), Location(7, 3)))
	);
	assert_eq!(f("1 2 3"), Ok(vec![Value::Int(1), Value::Int(2), Value::Int(3),]));
	assert_eq!(f("1 sub"), Err(Error("program expected [Int]".to_owned(), Location(0, 0))));
	assert_eq!(f("1 - 2"), Ok(vec![Value::Int(-1)]));
	assert_eq!(f("1 2 sub"), Ok(vec![Value::Int(-1)]));
	assert_eq!(f("1 - 2 - 3"), Ok(vec![Value::Int(-4)]));
	assert_eq!(f("1 == 2"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("1 != 2"), Ok(vec![Value::Bool(true)]));
	assert_eq!(f("1 > 2"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("(2 mul)@3"), Ok(vec![Value::Int(6)]));
	assert_eq!(f("{2 * 3} call"), Ok(vec![Value::Int(6)]));
	assert_eq!(f("if true 1"), Ok(vec![Value::Some(Box::new(Value::Int(1)))]));
	assert_eq!(f("if true 1 else 2"), Ok(vec![Value::Int(1)]));
	assert_eq!(f("i = 1 + 2 i"), Ok(vec![Value::Int(3)]));
	assert_eq!(f("{i = 1 + 2} call i"), Err(Error("i not found".to_owned(), Location(17, 1))));
	assert_eq!(f("not true"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("_not_@true"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("1 nop"), Ok(vec![Value::Int(1)]));
	assert_eq!(f("1 dup"), Ok(vec![Value::Int(1), Value::Int(1)]));
	assert_eq!(f("1 drop"), Ok(vec![]));
	assert_eq!(f("true drop 1 drop"), Ok(vec![]));
	assert_eq!(f("true and false"), Ok(vec![Value::Bool(false)]));
	assert_eq!(f("true or  false"), Ok(vec![Value::Bool(true)]));
	assert_eq!(f("17 5 mod"), Ok(vec![Value::Int(2)]));
	assert_eq!(f("mod@(5 2)"), Ok(vec![Value::Int(1)]));
	assert_eq!(f("if false 1 else if true 2 else 3"), Ok(vec![Value::Int(2)]));
}
