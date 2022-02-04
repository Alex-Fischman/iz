mod parser;
mod tokenizer;
mod typer;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).unwrap();
	let tokens = tokenizer::tokenize(file);
	let ast = parser::parse(&tokens);
	let typed = match typer::annotate(&ast) {
		Ok(t) => t,
		Err(e) => {
			println!("{}", e);
			return;
		}
	};
	println!("{:?}", typed);
}

#[derive(Clone, Copy)]
pub enum Assoc {
	Left = 0,
	Right = 1,
}

pub const BRACKETS: [(char, char); 3] = [('(', ')'), ('{', '}'), ('[', ']')];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("_ineg_", 4))];
pub const STATEMENTS: [(&str, (&str, u8)); 1] = [("if", ("_if_", 1))];
pub const INFIXES: [(&str, (&str, u8, Assoc)); 4] = [
	("+", ("_iadd_", 2, Assoc::Left)),
	("-", ("_isub_", 2, Assoc::Left)),
	("*", ("_imul_", 3, Assoc::Left)),
	("@", ("@", 10, Assoc::Left)),
	// ("else", ("else_", 1, Assoc::Right)),
];
