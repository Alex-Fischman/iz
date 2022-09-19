use crate::parse::Parsed;
use crate::parse::Tree;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
	Int(i64),
	Bool(bool),
	String(String),
}

impl Value {
	fn to_int(&self) -> Result<i64, String> {
		match self {
			Value::Int(i) => Ok(*i),
			v => Err(format!("expected int, found {:?}", v)),
		}
	}

	fn to_bool(&self) -> Result<bool, String> {
		match self {
			Value::Bool(b) => Ok(*b),
			v => Err(format!("expected bool, found {:?}", v)),
		}
	}

	fn to_string(&self) -> Result<&str, String> {
		match self {
			Value::String(s) => Ok(s),
			v => Err(format!("expected string, found {:?}", v)),
		}
	}
}

pub fn interpret(trees: &[Tree], names: &[String]) -> Result<Vec<Value>, String> {
	fn interpret(
		trees: &[Tree],
		names: &[String],
		stack: &mut Vec<Value>,
	) -> Result<(), String> {
		fn pop(stack: &mut Vec<Value>) -> Result<Value, String> {
			stack.pop().ok_or_else(|| "no value on stack".to_owned())
		}
		for tree in trees {
			interpret(&tree.children, names, stack)?;
			match &tree.data {
				Parsed::Name(i) => match names[*i].as_str() {
					"true" => stack.push(Value::Bool(true)),
					"false" => stack.push(Value::Bool(false)),
					"add" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Int(a + b));
					}
					"sub" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Int(a - b));
					}
					"mul" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Int(a * b));
					}
					"eq" => {
						let (b, a) = (pop(stack)?, pop(stack)?);
						stack.push(Value::Bool(match a {
							Value::Int(a) => a == b.to_int()?,
							Value::Bool(a) => a == b.to_bool()?,
							Value::String(a) => a == b.to_string()?,
						}));
					}
					"ne" => {
						let (b, a) = (pop(stack)?, pop(stack)?);
						stack.push(Value::Bool(match a {
							Value::Int(a) => a != b.to_int()?,
							Value::Bool(a) => a != b.to_bool()?,
							Value::String(a) => a != b.to_string()?,
						}));
					}
					"lt" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Bool(a < b));
					}
					"gt" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Bool(a > b));
					}
					"le" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Bool(a <= b));
					}
					"ge" => {
						let (b, a) = (pop(stack)?.to_int()?, pop(stack)?.to_int()?);
						stack.push(Value::Bool(a >= b));
					}
					"call" | "assign" | "_if_" | "_else_" | "_while_" => todo!(),
					s => Err(format!("unknown symbol {}", s))?,
				},
				Parsed::String(s) => stack.push(Value::String(s.clone())),
				Parsed::Number(n) => stack.push(Value::Int(*n)),
				Parsed::Brackets(_b) => todo!(),
			}
		}
		Ok(())
	}
	let mut stack = vec![];
	interpret(trees, names, &mut stack)?;
	Ok(stack)
}

#[test]
fn interpret_test() {
	use crate::parse::parse;
	use crate::tokenize::tokenize;
	let f = |s: &str| {
		let chars: Vec<char> = s.chars().collect();
		let tokens = tokenize(&chars).unwrap();
		let (trees, names) = parse(&tokens).unwrap();
		interpret(&trees, &names)
	};
	assert_eq!(f("true 1 sub"), Err("expected int, found Bool(true)".to_owned()));
	assert_eq!(f("1 sub"), Err("no value on stack".to_owned()));
	assert_eq!(f("1 - 2"), Ok(vec![Value::Int(-1)]));
	assert_eq!(f("1 > 2"), Ok(vec![Value::Bool(false)]));
}
