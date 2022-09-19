use crate::parse::Parsed;
use crate::parse::Tree;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
	Int(i64),
	Bool(bool),
	String(String),
}

pub fn interpret(trees: &[Tree], names: &[String]) -> Result<Vec<Value>, String> {
	fn interpret(
		trees: &[Tree],
		names: &[String],
		stack: &mut Vec<Value>,
	) -> Result<(), String> {
		fn pop_int(stack: &mut Vec<Value>) -> Result<i64, String> {
			match stack.pop() {
				Some(Value::Int(i)) => Ok(i),
				Some(v) => Err(format!("expected int, found {:?}", v)),
				None => Err("expected int, found nothing".to_owned()),
			}
		}
		for tree in trees {
			interpret(&tree.children, names, stack)?;
			match &tree.data {
				Parsed::Name(i) => match names[*i].as_str() {
					"true" => stack.push(Value::Bool(true)),
					"false" => stack.push(Value::Bool(false)),
					"add" => {
						let (a, b) = (pop_int(stack)?, pop_int(stack)?);
						stack.push(Value::Int(a + b));
					}
					"sub" => {
						let (a, b) = (pop_int(stack)?, pop_int(stack)?);
						stack.push(Value::Int(b - a));
					}
					"mul" => {
						let (a, b) = (pop_int(stack)?, pop_int(stack)?);
						stack.push(Value::Int(a * b));
					}
					"call" | "eq" | "ne" | "lt" | "gt" | "le" | "ge" | "assign"
					| "_if_" | "_else_" | "_while_" => todo!(),
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
	assert_eq!(f("true 1 sub"), Err("expected int, found Bool(true)".to_owned()),);
	assert_eq!(f("1 sub"), Err("expected int, found nothing".to_owned()),);
	assert_eq!(f("1 - 2"), Ok(vec![Value::Int(-1)]),);
}
