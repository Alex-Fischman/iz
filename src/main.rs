type Token = String;

#[derive(Debug, PartialEq)]
enum Error {
	MissingArgument,
	CouldNotReadFile(String),
	MissingEndQuote,
	MissingCloseBracket(Token),
	CouldNotFindType(Token),
	TypeMismatch(Type, Type),
}

fn main() -> Result<(), Error> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or(Error::MissingArgument)?;
	let text =
		std::fs::read_to_string(file).or(Err(Error::CouldNotReadFile(file.to_string())))?;

	let tokens = tokenize(&text)?;
	let ast = parse(&tokens)?;
	let types = annotate(&ast)?;
	println!("{:#?}", types);

	Ok(())
}

fn tokenize(s: &str) -> Result<Vec<Token>, Error> {
	fn is_splitter(c: char) -> bool {
		c == '(' || c == ')' || c == '{' || c == '}' || c.is_whitespace()
	}
	let mut tokens: Vec<Token> = vec![];
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
	if in_string {
		Err(Error::MissingEndQuote)
	} else {
		Ok(tokens.into_iter().filter(|t| !t.chars().next().unwrap().is_whitespace()).collect())
	}
}

#[test]
fn test() {
	assert_eq!(
		tokenize("# Comment\n\"test \\\"str#ing\"\n toke)n1 # comment\n").unwrap(),
		["\"test \"str#ing\"", "toke", ")", "n1"]
	);
	assert_eq!(
		tokenize("# Comment\n\"test \\\"str#ing\n toke)n1 # comment\n"),
		Err(Error::MissingEndQuote),
	);
}

#[derive(Debug, PartialEq)]
enum AST {
	Token(Token),
	Block(Vec<AST>),
}

fn parse(tokens: &[Token]) -> Result<AST, Error> {
	fn consume(
		tokens: &[Token],
		index: &mut usize,
		end: Option<&str>,
	) -> Result<Vec<AST>, Error> {
		let mut v = vec![];
		while match end {
			Some(end) if *index >= tokens.len() => {
				Err(Error::MissingCloseBracket(end.to_string()))?
			}
			Some(end) => end != tokens[*index],
			None => *index < tokens.len(),
		} {
			*index += 1;
			v.push(match &*tokens[*index - 1] {
				"{" => AST::Block(consume(tokens, index, Some("}"))?),
				_ => AST::Token(tokens[*index - 1].clone()),
			});
		}
		*index += 1;
		Ok(v)
	}
	Ok(AST::Block(consume(&tokens, &mut 0, None)?))
}

#[test]
fn parser_test() {
	assert_eq!(
		parse(&tokenize("1 {3 4} {} 5 {6 {8}}").unwrap()).unwrap(),
		AST::Block(vec![
			AST::Token("1".to_string()),
			AST::Block(vec![AST::Token("3".to_string()), AST::Token("4".to_string()),]),
			AST::Block(vec![]),
			AST::Token("5".to_string()),
			AST::Block(vec![
				AST::Token("6".to_string()),
				AST::Block(vec![AST::Token("8".to_string())])
			])
		])
	);
	assert_eq!(
		parse(&tokenize("1 {3 4} { 5 {6 {7 {8}}}").unwrap()),
		Err(Error::MissingCloseBracket("}".to_string()))
	);
}

#[derive(Clone, Debug, PartialEq)]
enum Type {
	Int,
	Bool,
}

#[derive(Clone, Debug, PartialEq)]
struct IO(Vec<Type>, Vec<Type>);

#[derive(Debug, PartialEq)]
enum TypedAST {
	Token(IO, Token),
	Block(IO, Vec<TypedAST>),
}

fn annotate(ast: &AST) -> Result<TypedAST, Error> {
	fn annotate(
		ast: &AST,
		map: &mut std::collections::HashMap<String, IO>,
	) -> Result<TypedAST, Error> {
		Ok(match ast {
			AST::Token(t) => TypedAST::Token(
				match t.parse::<i64>() {
					Ok(_) => IO(vec![], vec![Type::Int]),
					Err(_) => match map.get(t) {
						Some(io) => io.clone(),
						None => Err(Error::CouldNotFindType(t.clone()))?,
					},
				},
				t.clone(),
			),
			AST::Block(v) => {
				let v = v
					.iter()
					.map(|ast| annotate(ast, map))
					.collect::<Result<Vec<TypedAST>, Error>>()?;
				let mut io = IO(vec![], vec![]);
				for ast in &v {
					let io_ = match ast {
						TypedAST::Token(io, _) => io,
						TypedAST::Block(io, _) => io,
					}
					.clone();
					for t in io_.0 {
						match io.1.pop() {
							Some(t_) if t == t_ => {}
							Some(t_) => Err(Error::TypeMismatch(t_, t))?,
							None => io.0.insert(0, t),
						}
					}
					io.1.extend(io_.1);
				}
				TypedAST::Block(io, v)
			}
		})
	}
	annotate(
		ast,
		&mut std::collections::HashMap::from([
			("true".to_string(), IO(vec![], vec![Type::Bool])),
			("false".to_string(), IO(vec![], vec![Type::Bool])),
			("add".to_string(), IO(vec![Type::Int, Type::Int], vec![Type::Int])),
			("sub".to_string(), IO(vec![Type::Int, Type::Int], vec![Type::Int])),
			("mul".to_string(), IO(vec![Type::Int, Type::Int], vec![Type::Int])),
		]),
	)
}

#[test]
fn typer_test() {
	assert_eq!(
		annotate(&parse(&tokenize("1 2 add 3 mul 4").unwrap()).unwrap()).unwrap(),
		TypedAST::Block(
			IO(vec![], vec![Type::Int, Type::Int]),
			vec![
				TypedAST::Token(IO(vec![], vec![Type::Int]), "1".to_string()),
				TypedAST::Token(IO(vec![], vec![Type::Int]), "2".to_string()),
				TypedAST::Token(
					IO(vec![Type::Int, Type::Int], vec![Type::Int]),
					"add".to_string()
				),
				TypedAST::Token(IO(vec![], vec![Type::Int]), "3".to_string()),
				TypedAST::Token(
					IO(vec![Type::Int, Type::Int], vec![Type::Int]),
					"mul".to_string()
				),
				TypedAST::Token(IO(vec![], vec![Type::Int]), "4".to_string()),
			]
		),
	);
	assert_eq!(
		annotate(&parse(&tokenize("asdf").unwrap()).unwrap()),
		Err(Error::CouldNotFindType("asdf".to_string()))
	);
	assert_eq!(
		annotate(&parse(&tokenize("true 1 add").unwrap()).unwrap()),
		Err(Error::TypeMismatch(Type::Bool, Type::Int)),
	);
}
