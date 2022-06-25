use std::fmt::Debug;

fn main() {
	if let Err(e) = run() {
		println!("error: {}", e);
	}
}

fn run() -> Result<(), String> {
	let args: Vec<String> = std::env::args().collect();
	let text = match args.get(1) {
		None => return Err("pass a file".to_string()),
		Some(file) => match std::fs::read_to_string(file) {
			Err(_) => return Err("could not read file".to_string()),
			Ok(text) => text,
		},
	};
	let tokens = tokenize(&text);
	let ast = parse(&tokens);
	println!("{:?}", ast);
	Ok(())
}

#[derive(Clone, PartialEq)]
struct Token {
	string: String,
	row: usize,
	col: usize,
}

impl Debug for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{:?}@{}:{}", self.string, self.row, self.col)
	}
}

fn tokenize(s: &str) -> Vec<Token> {
	fn is_bracket(c: char) -> bool {
		c == '(' || c == ')' || c == '{' || c == '}'
	}
	fn char_type(c: char) -> u8 {
		if c.is_whitespace() {
			0
		} else if is_bracket(c) {
			1
		} else {
			2
		}
	}
	let mut row = 1;
	let mut col = 1;
	let mut tokens: Vec<Token> = vec![];
	let mut in_comment = false;
	let mut in_string = false;
	let mut in_escape = false;
	for c in s.chars() {
		if in_escape {
			tokens.last_mut().unwrap().string.push(c);
			in_escape = false;
		} else if in_string && c == '\\' {
			in_escape = true;
		} else if in_string {
			tokens.last_mut().unwrap().string.push(c);
			if c == '"' {
				in_string = false;
			}
		} else if c == '"' {
			in_string = true;
			tokens.push(Token { string: c.to_string(), row, col });
		} else if in_comment {
			if c == '\n' {
				in_comment = false;
			}
		} else if c == '#' {
			in_comment = true;
		} else if tokens.is_empty()
			|| is_bracket(c)
			|| char_type(c) != char_type(tokens.last().unwrap().string.chars().next().unwrap())
		{
			tokens.push(Token { string: c.to_string(), row, col });
		} else {
			tokens.last_mut().unwrap().string.push(c);
		}
		if c == '\n' {
			row += 1;
			col = 1;
		} else {
			col += 1;
		}
	}
	tokens.into_iter().filter(|t| !t.string.chars().next().unwrap().is_whitespace()).collect()
}

#[test]
fn tokenizer_test() {
	let result = tokenize("# Comment\n\"test \\\"str#ing\"\n toke)n1 # comment\n");
	let target = ["\"test \"str#ing\"", "toke", ")", "n1"];
	assert_eq!(result.iter().map(|t| &t.string).collect::<Vec<_>>(), target);
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum List {
	Block,
	Group,
}

#[derive(PartialEq)]
enum Tree<Leaf, List> {
	Leaf(Leaf),
	List(List, Vec<Tree<Leaf, List>>),
}

impl<Leaf: Debug, List: Debug> Debug for Tree<Leaf, List> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tree::Leaf(l) => write!(f, "{:?}", l),
			Tree::List(l, v) => write!(f, "{:?} {:#?}", l, v),
		}
	}
}

struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
}

fn parse(tokens: &[Token]) -> Tree<Token, List> {
	fn parse(c: &mut Context) -> Tree<Token, List> {
		let t = c.tokens[c.index].clone();
		c.index += 1;
		if t.string == "(" {
			let mut v = vec![];
			while c.tokens[c.index].string != ")" {
				v.push(parse(c));
			}
			c.index += 1;
			Tree::List(List::Group, v)
		} else if t.string == "{" {
			let mut v = vec![];
			while c.tokens[c.index].string != "}" {
				v.push(parse(c));
			}
			c.index += 1;
			Tree::List(List::Block, v)
		} else {
			Tree::Leaf(t)
		}
	}
	let c = &mut Context { tokens, index: 0 };
	let mut v = vec![];
	while c.index < c.tokens.len() {
		v.push(parse(c));
	}
	Tree::List(List::Group, v)
}

#[test]
fn parser_test() {
	let leaf = |s: &str, col| Tree::Leaf(Token { string: s.to_string(), row: 1, col });
	let target = Tree::List(
		List::Group,
		vec![
			leaf("1", 1),
			Tree::List(List::Group, vec![leaf("2", 4), leaf("5", 6), leaf("-", 8)]),
			leaf("+", 11),
			leaf("6", 13),
			leaf("*", 15),
		],
	);
	assert_eq!(parse(&tokenize("1 (2 5 -) + 6 *")), target);
}
