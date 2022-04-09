mod parser;
mod tokenizer;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).unwrap();
	let tokens = tokenizer::tokenize(&std::fs::read_to_string(file).unwrap());
	let ast = parser::parse(&tokens);
	println!("{:?}", ast);
}

#[test]
fn tokenizer_test() {
	let result = tokenizer::tokenize("# Comment\n\"test \\\"str#ing\"\n token1 # comment\n");
	let target = ["\"test \"str#ing\"", "token1"];
	assert_eq!(result.iter().map(|t| &t.string).collect::<Vec<_>>(), target);
}

#[test]
fn parser_test() {
	use parser::*;
	let unit = |s: &str, col| AST::Leaf(tokenizer::Token { string: s.to_string(), row: 1, col });
	let result = parse(&tokenizer::tokenize("1+(2-5)*6"));
	let target = AST::List(
		Lists::Curly,
		vec![AST::Call(
			Box::new(unit("add", 2)),
			vec![
				unit("1", 1),
				AST::Call(
					Box::new(unit("mul", 8)),
					vec![
						AST::List(
							Lists::Round,
							vec![AST::Call(
								Box::new(unit("sub", 5)),
								vec![unit("2", 4), unit("5", 6)],
							)],
						),
						unit("6", 9),
					],
				),
			],
		)],
	);
	assert_eq!(result, target);
}
