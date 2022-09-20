use crate::parse::Parsed;
use crate::parse::Tree;
use crate::tokenize::Bracket;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
	Int(i64),
	Bool(bool),
	String(String),
	Group(Vec<Value<'a>>),
	Block(Vec<Tree<'a>>),
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
		}
	}
}

pub fn interpret<'a>(trees: &[Tree<'a>], names: &[String]) -> Result<Vec<Value<'a>>, String> {
	fn interpret<'a>(
		trees: &[Tree<'a>],
		names: &[String],
		stack: &mut Vec<Value<'a>>,
	) -> Result<(), String> {
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
		for tree in trees {
			match &tree.data {
				Parsed::Name(i) => {
					interpret(&tree.children, names, stack)?;
					match names[*i].as_str() {
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
						"call" | "assign" | "_if_" | "_else_" | "_while_" => todo!(),
						s => Err(format!("unknown symbol {}", s))?,
					}
				}
				Parsed::String(s) => stack.push(Value::String(s.clone())),
				Parsed::Number(n) => stack.push(Value::Int(*n)),
				Parsed::Brackets(b) => match b {
					Bracket::Round => interpret(&tree.children, names, stack)?,
					Bracket::Curly => stack.push(Value::Block(tree.children.clone())),
					Bracket::Square => {
						let mut s = vec![];
						interpret(&tree.children, names, &mut s)?;
						stack.push(Value::Group(s));
					}
				},
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
	let chars: Vec<char> = "true 1 sub".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let (trees, names) = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees, &names), Err("invalid sub args: true, 1".to_owned()));
	let chars: Vec<char> = "1 sub".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let (trees, names) = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees, &names), Err("no value on stack".to_owned()));
	let chars: Vec<char> = "1 - 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let (trees, names) = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees, &names), Ok(vec![Value::Int(-1)]));
	let chars: Vec<char> = "1 > 2".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let (trees, names) = parse(&tokens).unwrap();
	assert_eq!(interpret(&trees, &names), Ok(vec![Value::Bool(false)]));
	let chars: Vec<char> = "1 2 [3 4] 5 6".chars().collect();
	let tokens = tokenize(&chars).unwrap();
	let (trees, names) = parse(&tokens).unwrap();
	assert_eq!(
		interpret(&trees, &names),
		Ok(vec![
			Value::Int(1),
			Value::Int(2),
			Value::Group(vec![Value::Int(3), Value::Int(4)]),
			Value::Int(5),
			Value::Int(6)
		])
	);
}
