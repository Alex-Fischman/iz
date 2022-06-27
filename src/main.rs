fn main() {
	match std::env::args().collect::<Vec<String>>().get(1) {
		None => println!("pass a file"),
		Some(file) => match std::fs::read_to_string(file) {
			Err(_) => println!("could not read file"),
			Ok(text) => {
				let tokens = tokenize(&text);
				let ast = parse(&tokens);
				let typed = annotate(&ast);
				let stack = interpret(&typed);
				println!("{:#?}", stack);
			}
		},
	};
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
}

#[derive(Debug, PartialEq)]
enum AST {
	Leaf(String),
	List(List, Vec<AST>),
}

fn parse(tokens: &[String]) -> AST {
	fn consume(tokens: &[String], index: &mut usize, end: &str) -> Vec<AST> {
		let mut v = vec![];
		while *index < tokens.len() && tokens[*index] != end {
			*index += 1;
			v.push(match &*tokens[*index - 1] {
				"(" => AST::List(List::Group, consume(tokens, index, ")")),
				_ => AST::Leaf(tokens[*index - 1].clone()),
			});
		}
		*index += 1;
		v
	}
	AST::List(List::Group, consume(&tokens, &mut 0, ""))
}

#[test]
fn parser_test() {
	let leaf = |s: &str| AST::Leaf(s.to_string());
	let target = AST::List(
		List::Group,
		vec![
			leaf("1"),
			AST::List(List::Group, vec![leaf("2"), leaf("5"), leaf("-")]),
			leaf("+"),
			leaf("6"),
			leaf("*"),
		],
	);
	assert_eq!(parse(&tokenize("1 (2 5 -) + 6 *")), target);
}

#[derive(Debug, PartialEq)]
enum Type {
	Int,
	Str,
	Bool,
	Block(Vec<Type>, Vec<Type>),
}

#[derive(Debug, PartialEq)]
enum TypedAST {
	Leaf(String, Type),
	List(List, Vec<TypedAST>),
}

fn annotate(ast: &AST) -> TypedAST {
	match ast {
		AST::Leaf(s) => TypedAST::Leaf(
			s.clone(),
			match s.as_str() {
				s if s.chars().all(|c| c.is_numeric() || c == '-' || c == '_') => Type::Int,
				s if s.chars().next().unwrap() == '"' => Type::Str,
				"true" => Type::Bool,
				"false" => Type::Bool,
				"add" | "sub" => Type::Block(vec![Type::Int, Type::Int], vec![Type::Int]),
				_ => panic!("unknown token: {:?}", s),
			},
		),
		AST::List(l, v) => TypedAST::List(l.clone(), v.iter().map(annotate).collect()),
	}
}

fn interpret(ast: &TypedAST) -> Vec<i64> {
	fn interpret(ast: &TypedAST, stack: &mut Vec<i64>) {
		match ast {
			TypedAST::Leaf(s, t) => match (s.as_str(), t) {
				(s, Type::Int) => stack.push(s.parse::<i64>().unwrap()),
				("add", Type::Block(t0, t1))
					if *t0 == vec![Type::Int, Type::Int] && *t1 == vec![Type::Int] =>
				{
					let c = stack.pop().unwrap() + stack.pop().unwrap();
					stack.push(c)
				}
				("sub", Type::Block(t0, t1))
					if *t0 == vec![Type::Int, Type::Int] && *t1 == vec![Type::Int] =>
				{
					let c = stack.pop().unwrap() - stack.pop().unwrap();
					stack.push(c);
				}
				(s, t) => panic!("unknown command: {:?}: {:?}", s, t),
			},
			TypedAST::List(List::Group, v) => v.iter().for_each(|t| interpret(t, stack)),
		}
	}
	let mut stack = vec![];
	interpret(ast, &mut stack);
	stack
}
