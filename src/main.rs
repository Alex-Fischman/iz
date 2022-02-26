mod parser;
mod tokenizer;
mod typer;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).unwrap();
	let tokens = tokenizer::tokenize(&std::fs::read_to_string(file).unwrap());
	let ast = parser::parse(&tokens);
	let typed = match typer::annotate(&ast) {
		Ok(t) => t,
		Err(e) => {
			println!("{}", e);
			return;
		}
	};
	println!("{:#?}", typed);
}

#[derive(Clone, Copy)]
pub enum Assoc {
	Left = 0,
	Right = 1,
}

pub const BRACKETS: [(char, char); 3] = [('(', ')'), ('{', '}'), ('[', ']')];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("neg", 4))];
pub const STATEMENTS: [(&str, (&str, u8)); 1] = [("if", ("_if_", 1))];
pub const INFIXES: [(&str, (&str, u8, Assoc)); 5] = [
	("+", ("add", 2, Assoc::Left)),
	("-", ("sub", 2, Assoc::Left)),
	("*", ("mul", 3, Assoc::Left)),
	("@", ("@", 10, Assoc::Left)),
	("else", ("_else_", 1, Assoc::Right)),
];

#[test]
fn tokenizer_test() {
	let result = tokenizer::tokenize("a+5badjf-*sadfjas\n fajsdfl 9jkl- sadf");
	let target = ["a", "+", "5badjf", "-*", "sadfjas", "fajsdfl", "9jkl", "-", "sadf"];
	for (t, b) in result.iter().zip(target) {
		assert_eq!(t.string, b);
	}
}

#[test]
fn parser_test() {
	fn check_ast(a: &parser::AST, b: &parser::AST) {
		match (a, b) {
			(parser::AST::Token(a), parser::AST::Token(b)) => assert_eq!(a.string, b.string),
			(parser::AST::List(a, xs, c), parser::AST::List(b, ys, d)) => {
				assert_eq!(a.string, b.string);
				assert_eq!(c.string, d.string);
				for (x, y) in xs.iter().zip(ys) {
					check_ast(x, y);
				}
			}
			(parser::AST::Call(a, c), parser::AST::Call(b, d)) => {
				check_ast(a, b);
				check_ast(c, d);
			}
			(a, b) => panic!("{:?} and {:?} have different types", a, b),
		}
	}

	fn token(s: &str) -> parser::AST {
		parser::AST::Token(tokenizer::Token { string: s.to_string(), row: 0, col: 0 })
	}

	fn list(s: &str, xs: Vec<parser::AST>, t: &str) -> parser::AST {
		parser::AST::List(
			tokenizer::Token { string: s.to_string(), row: 0, col: 0 },
			xs,
			tokenizer::Token { string: t.to_string(), row: 0, col: 0 },
		)
	}

	fn call(f: parser::AST, x: parser::AST) -> parser::AST {
		parser::AST::Call(Box::new(f), Box::new(x))
	}

	let result = parser::parse(&tokenizer::tokenize("1+(2-5)*6"));
	let target = call(
		call(token("add"), token("1")),
		call(
			call(
				token("mul"),
				list("(", vec![call(call(token("sub"), token("2")), token("5"))], ")"),
			),
			token("6"),
		),
	);
	check_ast(&result, &target);
}
