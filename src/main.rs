mod compiler;
mod parser;
mod scoper;
mod tokenizer;
mod tree;
mod typer;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).unwrap();
	let tokens = tokenizer::tokenize(&std::fs::read_to_string(file).unwrap());
	let ast = parser::parse(&tokens);
	match typer::annotate(&ast) {
		Err(_) => {}
		Ok(typed) => match compiler::compile(&typed) {
			Err(_) => {}
			Ok(program) => println!("{:?}", compiler::interpret(&program)),
		},
	}

	let scoped = match scoper::scope(&ast) {
		Ok(t) => t,
		Err(e) => {
			println!("{}", e);
			return;
		}
	};
	println!("{:?}", scoped);
}

#[derive(Clone, Copy)]
pub enum Assoc {
	Left = 0,
	Right = 1,
}

pub const BRACKETS: [(char, char); 3] = [('(', ')'), ('{', '}'), ('[', ']')];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("neg", 5))];
pub const STATEMENTS: [(&str, (&str, u8)); 1] = [("if", ("_if_", 1))];
pub const INFIXES: [(&str, (&str, u8, Assoc)); 7] = [
	("->", ("func", 1, Assoc::Right)),
	("==", ("eql", 2, Assoc::Left)),
	("+", ("add", 3, Assoc::Left)),
	("-", ("sub", 3, Assoc::Left)),
	("*", ("mul", 4, Assoc::Left)),
	("@", ("@", 10, Assoc::Left)),
	("else", ("_else_", 1, Assoc::Right)),
];

#[test]
fn tokenizer_test() {
	let result = tokenizer::tokenize(
		"# Comment\n\"test \\\"str#ing\"\nif true 1 else 0 # another comment\na1s2d",
	);
	let target = ["{", "\"test \"str#ing\"", "if", "true", "1", "else", "0", "a1s2d", "}"];
	assert_eq!(result.len(), target.len());
	for (t, b) in result.iter().zip(target) {
		assert_eq!(t.string, b);
	}
}

#[test]
fn parser_test() {
	let token = |s: &str| tokenizer::Token { string: s.to_string(), row: 0, col: 0 };
	let unit = |s| parser::AST::Leaf(token(s), ());
	let result = parser::parse(&tokenizer::tokenize("1+(2-5)*6"));
	let target = parser::AST::List(
		token("{"),
		vec![parser::AST::call(
			parser::AST::call(unit("add"), unit("1"), ()),
			parser::AST::call(
				parser::AST::call(
					unit("mul"),
					parser::AST::List(
						token("("),
						vec![parser::AST::call(
							parser::AST::call(unit("sub"), unit("2"), ()),
							unit("5"),
							(),
						)],
						(),
					),
					(),
				),
				unit("6"),
				(),
			),
			(),
		)],
		(),
	);
	assert!(tree::Tree::compare(
		&result,
		&target,
		|a, b| a.string == b.string,
		|a, b| a.string == b.string,
		PartialEq::eq
	));
}

#[test]
fn typer_test() {
	use typer::*;
	let unit =
		|t| TypedAST::Leaf(tokenizer::Token { string: "".to_string(), row: 0, col: 0 }, t);
	let string = "if true 2 else 4";
	let result = annotate(&parser::parse(&tokenizer::tokenize(string))).unwrap();
	let target = TypedAST::List(
		Lists::Curly,
		vec![TypedAST::call(
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
		)],
		int(),
	);
	assert!(tree::Tree::compare(&result, &target, |_, _| true, PartialEq::eq, PartialEq::eq));
}

#[test]
fn compiler_test() {
	let string = "-7 - (3 - 12) == 2";
	let result = compiler::interpret(
		&compiler::compile(
			&typer::annotate(&parser::parse(&tokenizer::tokenize(string))).unwrap(),
		)
		.unwrap(),
	);
	assert_eq!(result, [compiler::Frame::Bool(true)]);
}
