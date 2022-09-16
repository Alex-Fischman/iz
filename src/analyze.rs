use crate::parse::AST;
use crate::tokenize::Location;

#[derive(Debug)]
pub struct Tree<'a> {
	location: &'a Location<'a>,
	children: Vec<Tree<'a>>,
}

pub fn analyze<'a>(asts: &'a [AST]) -> Result<Vec<Tree<'a>>, String> {
	fn convert<'a>(ast: &'a AST) -> Tree<'a> {
		match ast {
			AST::Ident(location) => Tree { location, children: vec![] },
			AST::String(_, location) => Tree { location, children: vec![] },
			AST::Number(_, location) => Tree { location, children: vec![] },
			AST::Brackets(_, location, v) => {
				Tree { location, children: v.iter().map(convert).collect() }
			}
			AST::Operator(_, location, v) => {
				Tree { location, children: v.iter().map(convert).collect() }
			}
		}
	}
	let trees: Vec<Tree> = asts.iter().map(convert).collect();
	Ok(trees)
}

#[test]
fn analyze_test() {}
