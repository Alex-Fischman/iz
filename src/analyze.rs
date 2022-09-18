use crate::parse::OPERATORS;
use crate::parse::Parsed;
use crate::parse::Tree;
use crate::tokenize::Bracket;

#[derive(Debug)]
pub enum NameData {
	Name(usize),
	String(String),
	Number(i64),
	Brackets(Bracket),
}

pub fn analyze<'a>(asts: &[Tree<'a, Parsed>]) -> (Vec<Tree<'a, NameData>>, Vec<String>) {
	fn name<'a>(asts: &[Tree<'a, Parsed>], names: &mut Vec<String>) -> Vec<Tree<'a, NameData>> {
		fn new_name(name: &str, names: &mut Vec<String>) -> NameData {
			NameData::Name(names.iter().position(|n| n == name).unwrap_or_else(|| {
				names.push(name.to_owned());
				names.len() - 1
			}))
		}
		asts.iter().map(|ast| Tree {
			data: match &ast.data {
				Parsed::Ident => new_name(&ast.location.to_chars().iter().collect::<String>(), names),
				Parsed::String(s) => NameData::String(s.clone()),
				Parsed::Number(n) => NameData::Number(*n),
				Parsed::Brackets(b) => NameData::Brackets(*b),
				Parsed::Operator(i, j) => new_name(OPERATORS[*i].0[*j].1, names),
			},
			location:ast.location,
			children: name(&ast.children, names),
		}).collect()
	}
	let mut names = vec![];
	let named = name(asts, &mut names);
	(named, names)
}
