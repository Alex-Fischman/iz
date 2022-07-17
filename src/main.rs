enum Error {
	MissingArgument,
	CouldNotReadFile(String),
}

impl std::fmt::Debug for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Error::MissingArgument => write!(f, "pass a file name as a command line argument"),
			Error::CouldNotReadFile(file) => write!(f, "could not read file \"{}\"", file),
		}
	}
}

fn main() -> Result<(), Error> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or(Error::MissingArgument)?;
	let text =
		std::fs::read_to_string(file).or(Err(Error::CouldNotReadFile(file.to_string())))?;

	let tokens = tokenize(&text);
	let ast = parse(&tokens);
	let typed = annotate(&ast);
	let stack = interpret(&typed);
	println!("{:#?}", stack);

	Ok(())
}

fn tokenize(s: &str) -> Vec<String> {
	fn is_splitter(c: char) -> bool {
		c == '(' || c == ')' || c == '{' || c == '}' || c.is_whitespace()
	}
	let mut tokens: Vec<String> = vec![];
	let mut in_escape = false;
	let mut in_string = false;
	let mut in_comment = false;
	for c in s.chars() {
		match c {
			_ if in_escape => {
				in_escape = false;
				tokens.last_mut().unwrap().push(c);
			}
			'\\' if in_string => in_escape = true,

			'"' if in_string => {
				in_string = false;
				tokens.last_mut().unwrap().push(c);
			}
			_ if in_string => tokens.last_mut().unwrap().push(c),
			'"' => {
				in_string = true;
				tokens.push(c.to_string());
			}

			'\n' if in_comment => in_comment = false,
			_ if in_comment => {}
			'#' => in_comment = true,

			_ if tokens.is_empty()
				|| is_splitter(c)
				|| is_splitter(tokens.last().unwrap().chars().next().unwrap()) =>
			{
				tokens.push(c.to_string())
			}
			_ => tokens.last_mut().unwrap().push(c),
		}
	}
	tokens.into_iter().filter(|t| !t.chars().next().unwrap().is_whitespace()).collect()
}

#[test]
fn tokenizer_test() {
	let result = tokenize("# Comment\n\"test \\\"str#ing\"\n toke)n1 # comment\n");
	let target = ["\"test \"str#ing\"", "toke", ")", "n1"];
	assert_eq!(result, target);
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum List {
	Group,
	Block,
	Array,
}

#[derive(Debug, PartialEq)]
enum AST {
	Token(String),
	List(List, Vec<AST>),
}

fn parse(tokens: &[String]) -> AST {
	fn consume(tokens: &[String], index: &mut usize, end: &str) -> Vec<AST> {
		let mut v = vec![];
		while *index < tokens.len() && tokens[*index] != end {
			*index += 1;
			v.push(match &*tokens[*index - 1] {
				"(" => AST::List(List::Group, consume(tokens, index, ")")),
				"{" => AST::List(List::Block, consume(tokens, index, "}")),
				"[" => AST::List(List::Array, consume(tokens, index, "]")),
				_ => AST::Token(tokens[*index - 1].clone()),
			});
		}
		*index += 1;
		v
	}
	AST::List(List::Group, consume(&tokens, &mut 0, ""))
}

#[test]
fn parser_test() {
	let leaf = |s: &str| AST::Token(s.to_string());
	let target = AST::List(
		List::Group,
		vec![
			leaf("1"),
			AST::List(List::Group, vec![leaf("2"), leaf("5"), leaf("sub")]),
			leaf("add"),
			leaf("6"),
			leaf("mul"),
		],
	);
	assert_eq!(parse(&tokenize("1 (2 5 sub) add 6 mul")), target);
}

#[derive(Clone, Debug)]
enum Type {
	Type,
	Int,
	Str,
	Bool,
	Group(Vec<Type>),
	Block(Box<Type>, Box<Type>),
	// Array(Box<Type>), todo
}

impl PartialEq for Type {
	fn eq(&self, other: &Type) -> bool {
		use crate::Type::*;
		match (self, other) {
			(Type, Type) | (Int, Int) | (Str, Str) | (Bool, Bool) => true,
			(Group(a), Group(b)) => a == b,
			(Block(a, c), Block(b, d)) => a == b && c == d,
			(Group(v), y) | (y, Group(v)) if v.len() == 1 && v[0] == *y => true,
			_ => false,
		}
	}
}

#[derive(Debug, PartialEq)]
enum TypedAST {
	Token(String, Type),
	List(List, Vec<TypedAST>, Type),
}

impl TypedAST {
	fn get_type(&self) -> &Type {
		match self {
			TypedAST::Token(_, t) => t,
			TypedAST::List(_, _, t) => t,
		}
	}
}

fn annotate(ast: &AST) -> TypedAST {
	match ast {
		AST::Token(s) => TypedAST::Token(
			s.clone(),
			match s.as_str() {
				s if s.chars().all(|c| c.is_numeric() || c == '-' || c == '_') => Type::Int,
				s if s.chars().next().unwrap() == '"' => Type::Str,
				"true" | "false" => Type::Bool,
				"add" | "sub" | "mul" => Type::Block(
					Box::new(Type::Group(vec![Type::Int, Type::Int])),
					Box::new(Type::Int),
				),
				"Type" | "Int" | "Str" | "Bool" => Type::Type,
				"Group" | "Block" | "Array" => todo!(),
				_ => panic!("unknown token: {:?}", s),
			},
		),
		AST::List(l, v) => {
			let v: Vec<TypedAST> = v.iter().map(annotate).collect();
			let t = match l {
				List::Group => Type::Group(v.iter().map(|x| x.get_type().clone()).collect()),
				List::Block => todo!(),
				List::Array => todo!(),
			};
			TypedAST::List(l.clone(), v, t)
		}
	}
}

#[test]
fn typer_test() {
	let test = |a, t| assert_eq!(annotate(&a).get_type(), &t);
	test(AST::Token("Str".to_string()), Type::Type);
	test(AST::Token("-123".to_string()), Type::Int);
	test(AST::Token("\"asf fdsa\"".to_string()), Type::Str);
	test(AST::Token("true".to_string()), Type::Bool);
	test(
		AST::List(List::Group, vec![AST::Token("1".to_string()), AST::Token("2".to_string())]),
		Type::Group(vec![Type::Int, Type::Int]),
	);
	test(
		AST::Token("add".to_string()),
		Type::Block(Box::new(Type::Group(vec![Type::Int, Type::Int])), Box::new(Type::Int)),
	);
}

fn interpret(ast: &TypedAST) -> Vec<i64> {
	fn interpret(ast: &TypedAST, stack: &mut Vec<i64>) {
		match ast {
			TypedAST::Token(s, t) => match (s.as_str(), t) {
				(s, Type::Int) => stack.push(s.parse::<i64>().unwrap()),
				("add", Type::Block(t0, t1))
					if **t0 == Type::Group(vec![Type::Int, Type::Int]) && **t1 == Type::Int =>
				{
					let c = stack.pop().unwrap() + stack.pop().unwrap();
					stack.push(c)
				}
				("sub", Type::Block(t0, t1))
					if **t0 == Type::Group(vec![Type::Int, Type::Int]) && **t1 == Type::Int =>
				{
					let c = stack.pop().unwrap() - stack.pop().unwrap();
					stack.push(c);
				}
				("mul", Type::Block(t0, t1))
					if **t0 == Type::Group(vec![Type::Int, Type::Int]) && **t1 == Type::Int =>
				{
					let c = stack.pop().unwrap() * stack.pop().unwrap();
					stack.push(c);
				}
				(s, t) => panic!("unknown command: {:?}: {:?}", s, t),
			},
			TypedAST::List(List::Group, v, _) => v.iter().for_each(|x| interpret(x, stack)),
			TypedAST::List(List::Block, _, _) => todo!(),
			TypedAST::List(List::Array, _, _) => todo!(),
		}
	}
	let mut stack = vec![];
	interpret(ast, &mut stack);
	stack
}

#[test]
fn interpreter_test() {
	let test = |a, b| assert_eq!(interpret(&annotate(&parse(&tokenize(a)))), b);
	test("1 2 add", vec![3]);
	test("1 2 sub", vec![1]);
	test("1 2 mul", vec![2]);
	test("1 1 (mul 2) add 5 sub", vec![2]); // is this good?
}
