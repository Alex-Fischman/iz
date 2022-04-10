mod parser;
mod tokenizer;
mod typer;

use parser::*;
use std::io::{Error, ErrorKind};
use tokenizer::*;
use typer::*;

fn main() -> Result<(), Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(Error::new(ErrorKind::Other, "pass a .iz file"))?;
	let tokens = tokenize(&std::fs::read_to_string(file)?);
	let ast = parse(&tokens);
	let typed = annotate(&ast)?;
	println!("{:?}", typed);
	Ok(())
}

#[test]
fn tokenizer_test() {
	let result = tokenize("# Comment\n\"test \\\"str#ing\"\n token1 # comment\n");
	let target = ["\"test \"str#ing\"", "token1"];
	assert_eq!(result.iter().map(|t| &t.string).collect::<Vec<_>>(), target);
}

#[test]
fn parser_test() {
	let unit = |s: &str, col| AST::Leaf(Token { string: s.to_string(), row: 1, col });
	println!("{:?}", tokenize("1+(2-5)*6"));
	let result = parse(&tokenize("1+(2-5)*6"));
	let target = AST::List(
		Lists::Block,
		vec![AST::List(
			Lists::Block,
			vec![
				unit("1", 1),
				AST::List(
					Lists::Block,
					vec![
						AST::List(
							Lists::Group,
							vec![AST::List(
								Lists::Block,
								vec![unit("2", 4), unit("5", 6), unit("sub", 5)],
							)],
						),
						unit("6", 9),
						unit("mul", 8),
					],
				),
				unit("add", 2),
			],
		)],
	);
	assert_eq!(result, target);
}

#[test]
fn typer_test() {
	todo!();
}
