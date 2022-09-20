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

impl<'a> Value<'a> {
	fn to_int(&self) -> Result<i64, String> {
		match self {
			Value::Int(i) => Ok(*i),
			v => Err(format!("expected int, found {:?}", v)),
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
		for tree in trees {
			if let Parsed::Brackets(b) = &tree.data {
				match b {
					Bracket::Round => interpret(&tree.children, names, stack)?,
					Bracket::Curly => stack.push(Value::Block(tree.children.clone())),
					Bracket::Square => {
						let mut s = vec![];
						interpret(&tree.children, names, &mut s)?;
						stack.push(Value::Group(s));
					}
				}
			} else {
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
							stack.push(Value::Bool(a == b));
							todo!("assert same type and not blocks");
						}
						"ne" => {
							let (b, a) = (pop(stack)?, pop(stack)?);
							stack.push(Value::Bool(a != b));
							todo!("assert same type and not blocks");
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
					Parsed::Brackets(_) => unreachable!(),
				}
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
	assert_eq!(interpret(&trees, &names), Err("expected int, found Bool(true)".to_owned()));
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
