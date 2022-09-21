use crate::parse::Parsed;
use crate::parse::Tree;
use crate::tokenize::Bracket;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
	Int(i64),
	Bool(bool),
	String(String),
	Group(Vec<Value<'a>>),
	Block(Block<'a>),
	None,
	Some(Box<Value<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Block<'a> {
	Code(Vec<Tree<'a>>),
	Lazy(Box<Value<'a>>),
}

impl<'a> std::fmt::Display for Value<'a> {
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

// use std::collections::HashMap;
// #[derive(Debug)]
// struct Context<'a, 'b> {
// 	last: Option<&'a mut Context<'a, 'b>>,
// 	vars: HashMap<String, Value<'b>>,
// }

// impl<'a, 'b> Context<'a, 'b> {
// 	fn set<'c>(&'c mut self, key: &str, value: Value<'b>) {
// 		fn find<'a, 'b, 'c>(
// 			context: &'c mut Context<'a, 'b>,
// 			key: &str,
// 		) -> Option<&'c mut Context<'a, 'b>> {
// 			if context.vars.contains_key(key) {
// 				Some(context)
// 			} else {
// 				match &mut context.last {
// 					Some(last) => find(last, key),
// 					None => None,
// 				}
// 			}
// 		}
// 		match find(self, key) {
// 			Some(context) => context,
// 			None => self,
// 		}
// 		.vars
// 		.insert(key.to_owned(), value);
// 	}
// }

pub fn interpret<'a>(trees: &[Tree<'a>]) -> Result<Vec<Value<'a>>, String> {
	fn interpret<'a>(trees: &[Tree<'a>], stack: &mut Vec<Value<'a>>) -> Result<(), String> {
		fn pop<'a>(stack: &mut Vec<Value<'a>>) -> Result<Value<'a>, String> {
			stack.pop().ok_or_else(|| "no value on stack".to_owned())
		}
		fn eq(a: &Value, b: &Value) -> Result<bool, String> {
			match (a, b) {
				(Value::Int(a), Value::Int(b)) => Ok(a == b),
				(Value::Bool(a), Value::Bool(b)) => Ok(a == b),
				(Value::String(a), Value::String(b)) => Ok(a == b),
				(Value::Group(a), Value::Group(b)) => {
					a.iter().zip(b).try_fold(true, |acc, (a, b)| Ok(acc && eq(a, b)?))
				}
				(b, a) => Err(format!("invalid eq args: {}, {}", a, b))?,
			}
		}
		fn call<'a>(b: &Block<'a>, stack: &mut Vec<Value<'a>>) -> Result<(), String> {
			match b {
				Block::Code(v) => interpret(v, stack),
				Block::Lazy(v) => {
					stack.push(*v.clone());
					Ok(())
				}
			}
		}
		fn to_block(v: Value) -> Block {
			match v {
				Value::Block(b) => b,
				v => Block::Lazy(Box::new(v)),
			}
		}
		for tree in trees {
			match &tree.data {
				Parsed::Name(i) => {
					interpret(&tree.children, stack)?;
					match i.as_str() {
						"true" => stack.push(Value::Bool(true)),
						"false" => stack.push(Value::Bool(false)),
						"add" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a + b)),
							(b, a) => Err(format!("invalid add args: {}, {}", a, b))?,
						},
						"sub" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a - b)),
							(b, a) => Err(format!("invalid sub args: {}, {}", a, b))?,
						},
						"mul" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Int(a * b)),
							(b, a) => Err(format!("invalid mul args: {}, {}", a, b))?,
						},
						"eq" => {
							let (b, a) = (pop(stack)?, pop(stack)?);
							stack.push(Value::Bool(eq(&a, &b)?));
						}
						"ne" => {
							let (b, a) = (pop(stack)?, pop(stack)?);
							stack.push(Value::Bool(!eq(&a, &b)?));
						}
						"lt" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a < b)),
							(b, a) => Err(format!("invalid lt args: {}, {}", a, b))?,
						},
						"gt" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a > b)),
							(b, a) => Err(format!("invalid gt args: {}, {}", a, b))?,
						},
						"le" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a <= b)),
							(b, a) => Err(format!("invalid le args: {}, {}", a, b))?,
						},
						"ge" => match (pop(stack)?, pop(stack)?) {
							(Value::Int(b), Value::Int(a)) => stack.push(Value::Bool(a >= b)),
							(b, a) => Err(format!("invalid ge args: {}, {}", a, b))?,
						},
						"call" => match pop(stack)? {
							Value::Block(b) => call(&b, stack)?,
							v => Err(format!("invalid call arg: {}", v))?,
						},
						"call_with" => match (pop(stack)?, pop(stack)?) {
							(x, Value::Block(b)) => {
								stack.push(x);
								call(&b, stack)?;
							}
							(u, v) => Err(format!("invalid call_with args: {}, {}", u, v))?,
						},
						"_if_" => match (pop(stack)?, pop(stack)?) {
							(b, Value::Bool(true)) => {
								call(&to_block(b), stack)?;
								let c = pop(stack)?;
								stack.push(Value::Some(Box::new(c)))
							}
							(_, Value::Bool(false)) => stack.push(Value::None),
							(b, a) => Err(format!("invalid if args: {}, {}", a, b))?,
						},
						"_else_" => match (pop(stack)?, pop(stack)?) {
							(_, Value::Some(a)) => stack.push(*a),
							(b, Value::None) => stack.push(b),
							(b, a) => Err(format!("invalid else args: {}, {}", a, b))?,
						},
						"assign" => todo!(),
						s => Err(format!("unknown symbol {}", s))?,
					}
				}
				Parsed::String(s) => stack.push(Value::String(s.clone())),
				Parsed::Number(n) => stack.push(Value::Int(*n)),
				Parsed::Brackets(Bracket::Round) => interpret(&tree.children, stack)?,
				Parsed::Brackets(Bracket::Curly) => {
					stack.push(Value::Block(Block::Code(tree.children.clone())))
				}
				Parsed::Brackets(Bracket::Square) => {
					let mut s = vec![];
					interpret(&tree.children, &mut s)?;
					stack.push(Value::Group(s));
				}
			}
		}
		Ok(())
	}
	let mut stack = vec![];
	interpret(trees, &mut stack)?;
	Ok(stack)
}

#[test]
fn interpret_test() {
	use crate::parse::parse;
	use crate::tokenize::tokenize;
	let chars: Vec<char> = "true 1 sub".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Err("invalid sub args: true, 1".to_owned()));
	let chars: Vec<char> = "1 sub".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Err("no value on stack".to_owned()));
	let chars: Vec<char> = "1 - 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(-1)]));
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
	let chars: Vec<char> = "{2 mul}@3 {2 * 3} call".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(6), Value::Int(6)]));
	let chars: Vec<char> = "if true 1".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Some(Box::new(Value::Int(1)))]));
	let chars: Vec<char> = "if true 1 else 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let trees = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees), Ok(vec![Value::Int(1)]));
}
