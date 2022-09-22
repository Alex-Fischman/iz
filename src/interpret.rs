use crate::parse::Parsed;
use crate::parse::Tree;
use crate::tokenize::Bracket;
use crate::Error;
use crate::Location;

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
		fn pop(stack: &mut Vec<Value>, location: Location) -> Result<Value, Error> {
			stack.pop().ok_or_else(|| Error::new("no value on stack", location))
		}
		fn eq(a: &Value, b: &Value, l: Location) -> Result<bool, Error> {
			match (a, b) {
				(Value::Int(a), Value::Int(b)) => Ok(a == b),
				(Value::Bool(a), Value::Bool(b)) => Ok(a == b),
				(Value::String(a), Value::String(b)) => Ok(a == b),
				(Value::Group(a), Value::Group(b)) => {
					a.iter().zip(b).try_fold(true, |acc, (a, b)| Ok(acc && eq(a, b, l)?))
				}
				_ => Err(Error::new("invalid eq args", l))?,
			}
		}
		fn call(stack: &mut Vec<Value>, context: &mut Context, location: Location) -> Result<(), Error> {
			match pop(stack, location)? {
				Value::Block(v) => {
					context.push(HashMap::new());
					let out = interpret(&v, stack, context);
					context.pop();
					out
				}
				v => {
					stack.push(v);
					Ok(())
				}
			}
		}
		fn swap(stack: &mut Vec<Value>, location: Location) -> Result<(), Error> {
			let (b, a) = (pop(stack, location)?, pop(stack, location)?);
			stack.push(b);
			stack.push(a);
			Ok(())
		}
		for tree in trees {
			match &tree.data {
				Parsed::Name(i) if i == "assign" => {
					let key = match tree.children.get(0) {
						Some(Tree { data: Parsed::Name(key), .. }) => key,
						Some(_) => Err(Error::new("invalid var name", tree.location))?,
						None => Err(Error::new("no var name", tree.location))?,
					};
					interpret(&tree.children[1..], stack, context)?;
					match context.iter_mut().find(|frame| frame.contains_key(key)) {
						Some(frame) => frame,
						None => context.last_mut().unwrap(),
					}
					.insert(key.to_owned(), pop(stack, tree.location)?);
				}
				Parsed::Name(i) => {
					interpret(&tree.children, stack, context)?;
					match i.as_str() {
						"true" => stack.push(Value::Bool(true)),
						"false" => stack.push(Value::Bool(false)),
						"add" => match (pop(stack, tree.location)?, pop(stack, tree.location)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a + b)),
							_ => Err(Error::new("invalid add args", tree.location))?,
						},
						"sub" => match (pop(stack, tree.location)?, pop(stack, tree.location)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a - b)),
							_ => Err(Error::new("invalid sub args", tree.location))?,
						},
						"mul" => match (pop(stack, tree.location)?, pop(stack, tree.location)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a * b)),
							_ => Err(Error::new("invalid mul args", tree.location))?,
						},
						"not" => match pop(stack, tree.location)? {
							Value::Bool(b) => stack.push(Value::Bool(!b)),
							_ => Err(Error::new("invalid not args", tree.location))?,
						},
						"eq" => {
							let (b, a) = (pop(stack, tree.location)?, pop(stack, tree.location)?);
							stack.push(Value::Bool(eq(&a, &b, tree.location)?));
						}
						"lt" => match (pop(stack, tree.location)?, pop(stack, tree.location)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a < b)),
							_ => Err(Error::new("invalid lt args", tree.location))?,
						},
						"gt" => match (pop(stack, tree.location)?, pop(stack, tree.location)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a > b)),
							_ => Err(Error::new("invalid gt args", tree.location))?,
						},
						"call" => call(stack, context, tree.location)?,
						"swap" => swap(stack, tree.location)?,
						"_if_" => {
							swap(stack, tree.location)?;
							match pop(stack, tree.location)? {
								Value::Bool(true) => {
									call(stack, context, tree.location)?;
									let c = pop(stack, tree.location)?;
									stack.push(Value::Some(Box::new(c)))
								}
								Value::Bool(false) => stack.push(Value::None),
								_ => Err(Error::new("invalid if args", tree.location))?,
							}
						}
						"_else_" => match (pop(stack, tree.location)?, pop(stack, tree.location)?) {
							(_, Value::Some(a)) => stack.push(*a),
							(b, Value::None) => stack.push(b),
							_ => Err(Error::new("invalid else args", tree.location))?,
						},
						key => {
							stack.push(
								context
									.iter()
									.find_map(|frame| frame.get(key))
									.ok_or_else(|| Error::new("var not found", tree.location))?
									.clone(),
							);
							call(stack, context, tree.location)?;
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
	let prelude: [(&str, &[&str]); 4] = [
		("swap_call", &["swap", "call"]),
		("ne", &["eq", "not"]),
		("le", &["gt", "not"]),
		("ge", &["lt", "not"]),
	];
	let prelude = prelude.into_iter().map(|(key, idents)| {
		(
			key.to_owned(),
			Value::Block(
				idents.iter().map(|name| crate::parse::ident(name, Location(0, 0))).collect(),
			),
		)
	});
	let mut stack = vec![];
	interpret(trees, &mut stack, &mut vec![prelude.collect()])?;
	Ok(stack)
}

#[test]
fn interpret_test() {
	use crate::parse::parse;
	use crate::tokenize::tokenize;
	let chars: Vec<char> = "true 1 sub".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Err(Error::new("invalid sub args", Location(7, 3))));
	let chars: Vec<char> = "1 sub".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Err(Error::new("no value on stack", Location(2, 3))));
	let chars: Vec<char> = "1 - 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(-1)]));
	let chars: Vec<char> = "1 != 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Bool(true)]));
	let chars: Vec<char> = "1 > 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Bool(false)]));
	let chars: Vec<char> = "1 2 [3 4] 5 6".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(
		interpret(&trees),
		Ok(vec![
			Value::Int(1),
			Value::Int(2),
			Value::Group(vec![Value::Int(3), Value::Int(4)]),
			Value::Int(5),
			Value::Int(6)
		])
	);
	let chars: Vec<char> = "{2 mul}@3".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(6)]));
	let chars: Vec<char> = "{2 * 3} call".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(6)]));
	let chars: Vec<char> = "if true 1".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Some(Box::new(Value::Int(1)))]));
	let chars: Vec<char> = "if true 1 else 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(1)]));
	let chars: Vec<char> = "i = 1 + 2 i".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(3)]));
	let chars: Vec<char> = "{i = 1 + 2} call i".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Err(Error::new("var not found", Location(17, 1))));
	let chars: Vec<char> = "!true".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Bool(false)]));
}
