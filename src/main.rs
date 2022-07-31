type Token = String;

#[derive(Debug, PartialEq)]
enum Error {
	MissingArgument,
	CouldNotReadFile(String),
	MissingEndQuote,
	MissingCloseBracket(Token),
	CouldNotFindType(Token),
	TypeMismatch(Type, Type),
	CouldNotFindOp(IO, Token),
}

fn main() -> Result<(), Error> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or(Error::MissingArgument)?;
	let text =
		std::fs::read_to_string(file).or(Err(Error::CouldNotReadFile(file.to_string())))?;

	let tokens = tokenize(&text)?;
	let ast = parse(&tokens)?;
	let typed_ast = annotate(&ast)?;
	let ir = compile(&typed_ast)?;
	println!("{:#?}", ir);

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
fn tokenizer_test() {
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
			Some(end) if *index < tokens.len() => end != tokens[*index],
			Some(end) => Err(Error::MissingCloseBracket(end.to_string()))?,
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
	Ok(match ast {
		AST::Token(t) => TypedAST::Token(
			match t.as_str() {
				_ if t.parse::<i64>().is_ok() => IO(vec![], vec![Type::Int]),
				"true" => IO(vec![], vec![Type::Bool]),
				"false" => IO(vec![], vec![Type::Bool]),
				"add" => IO(vec![Type::Int, Type::Int], vec![Type::Int]),
				"sub" => IO(vec![Type::Int, Type::Int], vec![Type::Int]),
				"mul" => IO(vec![Type::Int, Type::Int], vec![Type::Int]),
				_ => Err(Error::CouldNotFindType(t.clone()))?,
			},
			t.clone(),
		),
		AST::Block(v) => {
			let mut io = IO(vec![], vec![]);
			let v = v.iter().map(annotate).collect::<Result<Vec<TypedAST>, Error>>()?;
			for ast in &v {
				let IO(i, o) = match ast {
					TypedAST::Token(io, _) => io,
					TypedAST::Block(io, _) => io,
				};
				for t in i.clone() {
					match io.1.pop() {
						Some(t_) if t == t_ => {}
						Some(t_) => Err(Error::TypeMismatch(t_, t))?,
						None => io.0.insert(0, t),
					}
				}
				io.1.append(&mut o.clone());
			}
			TypedAST::Block(io, v)
		}
	})
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

#[derive(Debug, PartialEq)]
enum Op {
	Bool(bool),
	Int(i64),
	IntAdd,
	IntSub,
	IntMul,
	BlockStart(usize),
	BlockEnd(usize),
}

fn compile(typed_ast: &TypedAST) -> Result<Vec<Op>, Error> {
	fn compile(typed_ast: &TypedAST, id: &mut usize) -> Result<Vec<Op>, Error> {
		Ok(match typed_ast {
			TypedAST::Token(io, t) => {
				vec![match (t.as_str(), io.0.as_slice(), io.1.as_slice()) {
					("true", [], [Type::Bool]) => Op::Bool(true),
					("false", [], [Type::Bool]) => Op::Bool(false),
					(i, [], [Type::Int]) if i.parse::<i64>().is_ok() => {
						Op::Int(i.parse().unwrap())
					}
					("add", [Type::Int, Type::Int], [Type::Int]) => Op::IntAdd,
					("sub", [Type::Int, Type::Int], [Type::Int]) => Op::IntSub,
					("mul", [Type::Int, Type::Int], [Type::Int]) => Op::IntMul,
					_ => Err(Error::CouldNotFindOp(io.clone(), t.clone()))?,
				}]
			}
			TypedAST::Block(_io, v) => {
				let label = *id;
				*id += 1;
				let mut out = vec![Op::BlockStart(label)];
				for typed_ast in v {
					out.append(&mut compile(typed_ast, id)?);
				}
				out.push(Op::BlockEnd(label));
				out
			}
		})
	}
	compile(typed_ast, &mut 0)
}

#[test]
fn compiler_test() {
	assert_eq!(
		compile(&annotate(&parse(&tokenize("1 2 add 3 {mul 4}").unwrap()).unwrap()).unwrap()),
		Ok(vec![
			Op::BlockStart(0),
			Op::Int(1),
			Op::Int(2),
			Op::IntAdd,
			Op::Int(3),
			Op::BlockStart(1),
			Op::IntMul,
			Op::Int(4),
			Op::BlockEnd(1),
			Op::BlockEnd(0),
		]),
	);
	assert_eq!(
		compile(&TypedAST::Token(IO(vec![], vec![]), "asdf".to_string())),
		Err(Error::CouldNotFindOp(IO(vec![], vec![]), "asdf".to_string()))
	);
}
