use crate::parse::Ast;
use crate::parse::Tree;
use crate::tokenize::Bracket;

#[derive(Debug)]
enum NameData {
	Name(usize),
	String(String),
	Number(i64),
	Bracket(Bracket),
}

fn analyze<'a>(_asts: &[Ast]) -> Result<Vec<Tree<'a, (NameData, Vec<String>)>>, String> {
	todo!()
}
