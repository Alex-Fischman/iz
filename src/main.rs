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
	let result = tokenizer::tokenize(
		"# Comment\n\"test \\\"str#ing\"\nif true 1 else 0 # another comment\na1s2d",
	);
	let target = ["\"test \"str#ing\"", "if", "true", "1", "else", "0", "a1s2d"];
	assert_eq!(result.len(), target.len());
	for (t, b) in result.iter().zip(target) {
		assert_eq!(t.string, b);
	}
}

#[test]
fn parser_test() {
	use parser::*;
	let token = |s: &str| tokenizer::Token { string: s.to_string(), row: 0, col: 0 };
	let unit = |s| AST::Leaf(token(s));
	let result = parse(&tokenizer::tokenize("1+(2-5)*6"));
	let target = AST::List(
		Lists::Curly,
		vec![AST::Call(
			Box::new(unit("add")),
			vec![
				unit("1"),
				AST::Call(
					Box::new(unit("mul")),
					vec![
						AST::List(
							Lists::Round,
							vec![AST::Call(Box::new(unit("sub")), vec![unit("2"), unit("5")])],
						),
						unit("6"),
					],
				),
			],
		)],
	);
	compare(&result, &target);
	fn compare(a: &AST, b: &AST) {
		match (a, b) {
			(AST::Leaf(a), AST::Leaf(b)) => assert_eq!(a.string, b.string),
			(AST::List(a, b), AST::List(c, d)) => {
				assert_eq!(a, c);
				for (b, d) in b.iter().zip(d) {
					compare(b, d);
				}
			}
			(AST::Call(a, b), AST::Call(c, d)) => {
				compare(a, c);
				for (b, d) in b.iter().zip(d) {
					compare(b, d);
				}
			}
			(a, b) => panic!("{:?} != {:?}", a, b),
		}
	}
}
