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

	let token = |s| AST::Token(tokenizer::Token::new(s));
	let list = |s, xs, t| AST::List(tokenizer::Token::new(s), xs, tokenizer::Token::new(t));

	let result = parser::parse(&tokenizer::tokenize("1+(2-5)*6"));
	let target = AST::call(
		AST::call(token("add"), token("1")),
		AST::call(
			AST::call(
				token("mul"),
				list("(", vec![AST::call(AST::call(token("sub"), token("2")), token("5"))], ")"),
			),
			token("6"),
		),
	);
	check_ast(&result, &target);
}

#[test]
fn typer_test() {
	use typer::Type;
	use typer::TypedAST;

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

	let unit = |t| TypedAST::Token(tokenizer::Token::new(""), t);

	let result = typer::annotate(&parser::parse(&tokenizer::tokenize("1+1"))).unwrap();
	let target = TypedAST::call(
		TypedAST::call(
			unit(Type::func(
				Type::data("int"),
				Type::func(Type::data("int"), Type::data("int")),
			)),
			unit(Type::data("int")),
			Type::func(Type::data("int"), Type::data("int")),
		),
		unit(Type::data("int")),
		Type::data("int"),
	);

	check_types(&result, &target)
}
