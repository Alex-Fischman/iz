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
	let result = tokenizer::tokenize(
		"{
		# Comment
		\"test string\"
		if true 1 else 0 # another comment
		a1s2d
	}",
	);
	let target = ["{", "\"test string\"", "if", "true", "1", "else", "0", "a1s2d", "}"];
	assert_eq!(result.len(), target.len());
	for (t, b) in result.iter().zip(target) {
		assert_eq!(t.string, b);
	}
}

#[test]
fn parser_test() {
	use parser::AST;

	fn check_ast(a: &AST, b: &AST) {
		match (a, b) {
			(AST::Token(a), AST::Token(b)) => assert_eq!(a.string, b.string),
			(AST::List(a, xs, c), AST::List(b, ys, d)) => {
				assert_eq!(a.string, b.string);
				assert_eq!(c.string, d.string);
				for (x, y) in xs.iter().zip(ys) {
					check_ast(x, y);
				}
			}
			(AST::Call(a, c), AST::Call(b, d)) => {
				check_ast(a, b);
				check_ast(c, d);
			}
			(a, b) => panic!("{:?} and {:?} have different types", a, b),
		}
	}

	let token = |s: &str| tokenizer::Token { string: s.to_string(), row: 0, col: 0 };
	let unit = |s| AST::Token(token(s));
	let list = |s, xs, t| AST::List(token(s), xs, token(t));

	let result = parser::parse(&tokenizer::tokenize("1+(2-5)*6"));
	let target = AST::call(
		AST::call(unit("add"), unit("1")),
		AST::call(
			AST::call(
				unit("mul"),
				list("(", vec![AST::call(AST::call(unit("sub"), unit("2")), unit("5"))], ")"),
			),
			unit("6"),
		),
	);
	check_ast(&result, &target);
}

#[test]
fn typer_test() {
	use typer::*;

	fn check_types(a: &TypedAST, b: &TypedAST) {
		assert_eq!(a.get_type(), b.get_type());
		match (a, b) {
			(TypedAST::Token(_, _), TypedAST::Token(_, _)) => {}
			(TypedAST::List(_, xs, _, _), TypedAST::List(_, ys, _, _)) => {
				xs.iter().zip(ys).for_each(|(x, y)| check_types(x, y))
			}
			(TypedAST::Call(a, c, _), TypedAST::Call(b, d, _)) => {
				check_types(a, b);
				check_types(c, d);
			}
			(a, b) => panic!("{:?} and {:?} have different types", a, b),
		}
	}

	let token = |s: &str| tokenizer::Token { string: s.to_string(), row: 0, col: 0 };
	let unit = |t| TypedAST::Token(token(""), t);

	let string = "if true 2 else 4";
	let result = typer::annotate(&parser::parse(&tokenizer::tokenize(string))).unwrap();
	let target = TypedAST::call(
		TypedAST::call(
			unit(func(option(int()), func(int(), int()))),
			TypedAST::call(
				TypedAST::call(
					unit(func(boolean(), func(int(), option(int())))),
					unit(boolean()),
					func(int(), option(int())),
				),
				unit(int()),
				option(int()),
			),
			func(int(), int()),
		),
		unit(int()),
		int(),
	);

	check_types(&result, &target)
}
