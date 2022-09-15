#[derive(Debug)]
enum Type {
	Int,
	String,
	Bool,
}

#[derive(Debug)]
pub struct IO(Vec<Type>, Vec<Type>);

#[derive(Debug)]
pub struct AST<'a, T> {
	data: T,
	from: &'a crate::parse::AST<'a>,
	children: Vec<AST<'a, T>>,
}

pub fn analyze<'a>(_asts: &[crate::parse::AST<'a>]) -> Result<Vec<AST<'a, IO>>, String> {
	todo!()
}

#[test]
fn analyze_test() {
	// extra inputs test
	// extra outputs test
	// test for each Type variant
	// type vars test
	// combo test
}
