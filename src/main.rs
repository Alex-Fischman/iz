mod parser;
mod tokenizer;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).unwrap();
	let tokens = tokenizer::tokenize(file);
	let ast = parser::parse(&tokens);
	println!("{:?}", ast);
}

#[derive(Clone, Copy)]
pub enum Assoc {
	Left = 0,
	Right = 1,
}

pub const BRACKETS: [(char, char); 3] = [('(', ')'), ('{', '}'), ('[', ']')];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("neg", 4))];
pub const STATEMENTS: [(&str, (&str, u8)); 1] = [("if", ("if_", 1))];
pub const INFIXES: [(&str, (&str, u8, Assoc)); 4] = [
	("+", ("add", 2, Assoc::Left)),
	("-", ("sub", 2, Assoc::Left)),
	("*", ("mul", 3, Assoc::Left)),
	("else", ("else_", 1, Assoc::Right)),
];
