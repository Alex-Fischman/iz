mod parser;
mod tokenizer;
mod typer;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).unwrap();
	let tokens = tokenizer::tokenize(file);
	let ast = parser::parse(&tokens);
	let typed = typer::annotate(&ast);
	println!("{:?}", typed);
}

#[derive(Clone, Copy)]
pub enum Assoc {
	Left = 0,
	Right = 1,
}

pub const BRACKETS: [(char, char); 3] = [('(', ')'), ('{', '}'), ('[', ']')];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("neg_int", 4))];
pub const STATEMENTS: [(&str, (&str, u8)); 1] = [("if", ("if_", 1))];
pub const INFIXES: [(&str, (&str, u8, Assoc)); 4] = [
	("+", ("add_int", 2, Assoc::Left)),
	("-", ("sub_int", 2, Assoc::Left)),
	("*", ("mul_int", 3, Assoc::Left)),
	("else", ("else_", 1, Assoc::Right)),
];
