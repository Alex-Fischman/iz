mod compiler;
mod interpreter;
mod parser;
mod tokenizer;
mod typer;

use compiler::*;
use interpreter::*;
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
	let program = compile(&typed)?;
	let output = interpret(&program);
	println!("{:?}", output);
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
	let result = parse(&tokenize("1+{2-5}*6"));
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
							Lists::Block,
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
	let unit = |s: &str, col, t| TypedAST::Leaf(Token { string: s.to_string(), row: 1, col }, t);
	let result = annotate(&parse(&tokenize("1 - 5 == -4"))).unwrap();
	let target = TypedAST::List(
		Lists::Block,
		vec![TypedAST::List(
			Lists::Block,
			vec![
				TypedAST::List(
					Lists::Block,
					vec![
						unit("1", 1, (vec![], vec![Type::Int])),
						unit("5", 5, (vec![], vec![Type::Int])),
						unit("sub", 3, (vec![Type::Int, Type::Int], vec![Type::Int])),
					],
					(vec![], vec![Type::Int]),
				),
				TypedAST::List(
					Lists::Block,
					vec![
						unit("4", 11, (vec![], vec![Type::Int])),
						unit("neg", 10, (vec![Type::Int], vec![Type::Int])),
					],
					(vec![], vec![Type::Int]),
				),
				unit("eql", 7, (vec![Type::Int, Type::Int], vec![Type::Bool])),
			],
			(vec![], vec![Type::Bool]),
		)],
		(vec![], vec![Type::Bool]),
	);
	assert_eq!(result, target);
}

#[test]
fn compiler_test() {
	let result = compile(&annotate(&parse(&tokenize("5*2-19== -9"))).unwrap()).unwrap();
	use Op::*;
	let target = [PushI(5), PushI(2), MulI, PushI(19), SubI, PushI(9), NegI, EqlI];
	assert_eq!(result, target);
}

#[test]
fn interpreter_test() {
	let program = "true 1 branch true true true true";
	let result = interpret(&compile(&annotate(&parse(&tokenize(program))).unwrap()).unwrap());
	let target = [1, 1, 1];
	assert_eq!(result, target);
}
