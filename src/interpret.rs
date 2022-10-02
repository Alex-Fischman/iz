use crate::analyze::{Tree, Type};
use crate::parse::Parsed;
use crate::tokenize::Bracket;
use crate::{Error, Location};
type Context = crate::Context<String, Value>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Data(Vec<u8>),
	Block(Vec<Tree>, Context),
	Enum(Box<Value>),
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Value::Data(data) => write!(f, "{:?}", data),
			Value::Block(..) => write!(f, "Block"),
			Value::Enum(v) => write!(f, "Enum({})", v),
		}
	}
}

impl Value {
	fn int(i: i64) -> Value {
		Value::Data(i64::to_ne_bytes(i).to_vec())
	}

	fn r#true() -> Value {
		Value::Data(vec![0x01])
	}

	fn r#false() -> Value {
		Value::Data(vec![0x00])
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
						"true" => stack.push(Value::r#true()),
						"false" => stack.push(Value::r#false()),
						"add" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Data(b), Value::Data(a)) => stack.push(Value::int(
								i64::from_ne_bytes(a.try_into().unwrap())
									+ i64::from_ne_bytes(b.try_into().unwrap()),
							)),
							_ => Err(Error("invalid _add_ args".to_owned(), l))?,
						},
						"sub" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Data(b), Value::Data(a)) => stack.push(Value::int(
								i64::from_ne_bytes(a.try_into().unwrap())
									- i64::from_ne_bytes(b.try_into().unwrap()),
							)),
							_ => Err(Error("invalid _sub_ args".to_owned(), l))?,
						},
						"mul" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Data(b), Value::Data(a)) => stack.push(Value::int(
								i64::from_ne_bytes(a.try_into().unwrap())
									* i64::from_ne_bytes(b.try_into().unwrap()),
							)),
							_ => Err(Error("invalid _mul_ args".to_owned(), l))?,
						},
						"eq" => {
							let (b, a) = (pop(stack, l)?, pop(stack, l)?);
							stack.push(Value::Data(vec![(a == b) as u8]));
						}
						"lt" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Data(b), Value::Data(a)) => stack.push(Value::Data(vec![
								(i64::from_ne_bytes(a.try_into().unwrap())
									< i64::from_ne_bytes(b.try_into().unwrap())) as u8,
							])),
							_ => Err(Error("invalid _lt_ args".to_owned(), l))?,
						},
						"gt" => match (pop(stack, l)?, pop(stack, l)?) {
							(Value::Data(b), Value::Data(a)) => stack.push(Value::Data(vec![
								(i64::from_ne_bytes(a.try_into().unwrap())
									> i64::from_ne_bytes(b.try_into().unwrap())) as u8,
							])),
							_ => Err(Error("invalid _gt_ args".to_owned(), l))?,
						},
						"_if_" => match (pop(stack, l)?, pop(stack, l)?) {
							(b, v) if v == Value::r#true() => {
								stack.push(Value::Enum(Box::new(b)))
							}
							(_, v) if v == Value::r#false() => {
								stack.push(Value::Enum(Box::new(Value::Data(vec![]))))
							}
							_ => Err(Error("invalid _if_ args".to_owned(), l))?,
						},
						"_else_" => match (pop(stack, l)?, pop(stack, l)?) {
							(b, Value::Enum(a)) if *a == Value::Data(vec![]) => stack.push(b),
							(_, Value::Enum(a)) => stack.push(*a),
							_ => Err(Error("invalid _else_ args".to_owned(), l))?,
						},
						"_while_" => {
							let (b, a) = (pop(stack, l)?, pop(stack, l)?);
							while {
								stack.push(a.clone());
								call(stack, types, l)?;
								match pop(stack, l)? {
									v if v == Value::r#true() => true,
									v if v == Value::r#false() => false,
									_ => Err(Error("invalid _while_ args".to_owned(), l))?,
								}
							} {
								stack.push(b.clone());
								call(stack, types, l)?;
							}
						}
						"print_int" => match pop(stack, l)? {
							Value::Data(v) => {
								print!("{}", i64::from_ne_bytes(v.try_into().unwrap()))
							}
							_ => Err(Error("invalid print_int args".to_owned(), l))?,
						},
						"print_string" => match pop(stack, l)? {
							Value::Data(v) => {
								print!("{}", std::str::from_utf8(&v).unwrap())
							}
							_ => Err(Error("invalid print_int args".to_owned(), l))?,
						},
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
				Parsed::String(s) => stack.push(Value::Data(s.as_bytes().to_vec())),
				Parsed::Number(n) => stack.push(Value::int(*n)),
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
	assert_eq!(f("1 2 3"), Ok(vec![Value::int(1), Value::int(2), Value::int(3),]));
	assert_eq!(f("1 sub"), Err(Error("program expected [Int]".to_owned(), Location(0, 0))));
	assert_eq!(f("1 - 2"), Ok(vec![Value::int(-1)]));
	assert_eq!(f("1 2 sub"), Ok(vec![Value::int(-1)]));
	assert_eq!(f("1 - 2 - 3"), Ok(vec![Value::int(-4)]));
	assert_eq!(f("1 == 2"), Ok(vec![Value::r#false()]));
	assert_eq!(f("1 != 2"), Ok(vec![Value::r#true()]));
	assert_eq!(f("1 > 2"), Ok(vec![Value::r#false()]));
	assert_eq!(f("(2 mul)@3"), Ok(vec![Value::int(6)]));
	assert_eq!(f("{2 * 3} call"), Ok(vec![Value::int(6)]));
	assert_eq!(f("if true 1"), Ok(vec![Value::Enum(Box::new(Value::int(1)))]));
	assert_eq!(f("if true 1 else 2"), Ok(vec![Value::int(1)]));
	assert_eq!(f("i = 1 + 2 i"), Ok(vec![Value::int(3)]));
	assert_eq!(f("{i = 1 + 2} call i"), Err(Error("i not found".to_owned(), Location(17, 1))));
	assert_eq!(f("not true"), Ok(vec![Value::r#false()]));
	assert_eq!(f("_not_@true"), Ok(vec![Value::r#false()]));
	assert_eq!(f("1 nop"), Ok(vec![Value::int(1)]));
	assert_eq!(f("1 dup"), Ok(vec![Value::int(1), Value::int(1)]));
	assert_eq!(f("1 drop"), Ok(vec![]));
	assert_eq!(f("true drop 1 drop"), Ok(vec![]));
	assert_eq!(f("true and false"), Ok(vec![Value::r#false()]));
	assert_eq!(f("true or  false"), Ok(vec![Value::r#true()]));
	assert_eq!(f("17 5 mod"), Ok(vec![Value::int(2)]));
	assert_eq!(f("mod@(5 2)"), Ok(vec![Value::int(1)]));
	assert_eq!(f("if false 1 else if true 2 else 3"), Ok(vec![Value::int(2)]));
}
