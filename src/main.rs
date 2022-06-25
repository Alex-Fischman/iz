mod parser;
mod tokenizer;

fn error(s: &str) -> std::io::Error {
	std::io::Error::new(std::io::ErrorKind::Other, s)
}

fn main() -> Result<(), std::io::Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(error("pass a .iz file"))?;
	let tokens = tokenizer::tokenize(&std::fs::read_to_string(file)?);
	let ast = parser::parse(&tokens);
	println!("{:?}", ast);
	Ok(())
}

#[test]
fn tokenizer_test() {
	let result = tokenizer::tokenize("# Comment\n\"test \\\"str#ing\"\n token1 # comment\n");
	let target = ["\"test \"str#ing\"", "token1"];
	assert_eq!(result.iter().map(|t| &t.string).collect::<Vec<_>>(), target);
}

#[test]
fn parser_test() {
	use parser::{AST, Lists};
	let unit = |s: &str, col| AST::Leaf(tokenizer::Token { string: s.to_string(), row: 1, col });
	let result = parser::parse(&tokenizer::tokenize("1 (2 5 -) + 6 *"));
	let target = AST::List(
		Lists::Group,
		vec![
			unit("1", 1),
			AST::List(Lists::Group, vec![
				unit("2", 4),
				unit("5", 6),
				unit("-", 8),
			]),
			unit("+", 11),
			unit("6", 13),
			unit("*", 15),
		],
	);
	assert_eq!(result, target);
}
