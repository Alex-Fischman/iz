#[derive(Debug, PartialEq)]
enum Error {
	MissingArgument,
	CouldNotReadFile(String),
	MissingEndQuote,
	MissingCloseBracket(String),
}

fn main() -> Result<(), Error> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or(Error::MissingArgument)?;
	let text =
		std::fs::read_to_string(file).or(Err(Error::CouldNotReadFile(file.to_string())))?;

	let tokens = tokenize(&text)?;
	let ir = parse(&tokens)?;
	println!("{:?}", ir);

	Ok(())
}

fn tokenize(s: &str) -> Result<Vec<String>, Error> {
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
	Token(String),
	Group(Vec<AST>),
	Block(Vec<AST>),
}

fn parse(tokens: &[String]) -> Result<AST, Error> {
	fn consume(
		tokens: &[String],
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
				"(" => AST::Group(consume(tokens, index, Some(")"))?),
				"{" => AST::Block(consume(tokens, index, Some("}"))?),
				_ => AST::Token(tokens[*index - 1].clone()),
			});
		}
		*index += 1;
		Ok(v)
	}
	Ok(AST::Group(consume(&tokens, &mut 0, None)?))
}

#[test]
fn parser_test() {
	assert_eq!(
		parse(&tokenize("1 {3 4} () 5 (6 {8})").unwrap()).unwrap(),
		AST::Group(vec![
			AST::Token("1".to_string()),
			AST::Block(vec![AST::Token("3".to_string()), AST::Token("4".to_string()),]),
			AST::Group(vec![]),
			AST::Token("5".to_string()),
			AST::Group(vec![
				AST::Token("6".to_string()),
				AST::Block(vec![AST::Token("8".to_string())])
			])
		])
	);
	assert_eq!(
		parse(&tokenize("1 {3 4} ( 5 (6 (7 {8}))").unwrap()),
		Err(Error::MissingCloseBracket(")".to_string()))
	);
}
