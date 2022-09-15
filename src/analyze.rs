use crate::parse::AST;

#[derive(Debug)]
pub struct Tree<'a> {
	ast: &'a AST<'a>,
	children: Vec<Tree<'a>>,
}

pub fn analyze<'a>(asts: &'a [AST]) -> Result<Vec<Tree<'a>>, String> {
	fn convert<'a>(ast: &'a AST) -> Tree<'a> {
		match ast {
			AST::Ident(..) => Tree { ast, children: vec![] },
			AST::String(..) => Tree { ast, children: vec![] },
			AST::Number(..) => Tree { ast, children: vec![] },
			AST::Brackets(.., v) => Tree { ast, children: v.iter().map(convert).collect() },
			AST::Operator(.., v) => Tree { ast, children: v.iter().map(convert).collect() }
		}
	}
	let trees: Vec<Tree> = asts.iter().map(convert).collect();
	Ok(trees)
}

#[test]
fn analyze_test() {
	
}
