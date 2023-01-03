fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let tokens = tokenizer(&chars);
	let trees = parser(&tokens);
	let tree = rewriter(&trees);
	let tree = typer(&tree);
	let code = compile(&tree);
	let stack = run(&code);
	println!("{:?}", stack);
}

#[test]
fn test() {
	assert_eq!(
		tokenizer(&"1+2 0b10_0000 0xff \"a b\tc\nd\\\"\"".chars().collect::<Vec<char>>()),
		vec![
			Token::Number(1),
			Token::Identifier("+".to_string()),
			Token::Number(2),
			Token::Number(32),
			Token::Number(255),
			Token::String("a b\tc\nd\"".to_string()),
		]
	);
	let parse_test = "if false 1 else if true 2 else 3\n# asdf\na := 1 + (2) * 3";
	assert_eq!(
		rewriter(&parser(&tokenizer(&parse_test.chars().collect::<Vec<char>>()))),
		Rewritten::Block(vec![
			Rewritten::Number(3),
			Rewritten::Number(2),
			Rewritten::Identifier("true".to_string()),
			Rewritten::Identifier("_if_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Identifier("_else_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Number(1),
			Rewritten::Identifier("false".to_string()),
			Rewritten::Identifier("_if_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Identifier("_else_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Number(3),
			Rewritten::Number(2),
			Rewritten::Identifier("mul".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Number(1),
			Rewritten::Identifier("add".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Declaration(0),
		])
	);
	let typer_test = "{1 2} call add call a := 2 a".chars().collect::<Vec<char>>();
	let ts = new_scope(None); // typer generates a floating scope
	let ts = new_scope(Some(ts));
	set_var(ts.clone(), 0, Int);
	assert_eq!(
		typer(&rewriter(&parser(&tokenizer(&typer_test)))),
		Typed::Block(
			vec![
				Typed::Block(
					vec![Typed::Number(1), Typed::Number(2)],
					Effect::new(vec![], vec![Int, Int]),
					new_scope(Some(ts.clone()))
				),
				Typed::Identifier(
					"call".to_string(),
					Effect::new(
						vec![Block(Effect::new(vec![], vec![Int, Int]))],
						vec![Int, Int]
					)
				),
				Typed::Identifier(
					"add".to_string(),
					Effect::function(vec![Int, Int], vec![Int])
				),
				Typed::Identifier(
					"call".to_string(),
					Effect::new(
						vec![Int, Int, Block(Effect::new(vec![Int, Int], vec![Int]))],
						vec![Int],
					),
				),
				Typed::Number(2),
				Typed::Assignment(0, Int),
				Typed::Variable(0, Int),
			],
			Effect::new(vec![], vec![Int, Int]),
			ts,
		)
	);
	let run_test: Vec<char> =
		"a := add@(1 2) 2 2 b := 0 b = add@() c := 3 + 2 {1 2} call a b c".chars().collect();
	let mut stack = Stack::new();
	[1i64, 2, 3, 4, 5].into_iter().for_each(|v| stack.push(v));
	assert_eq!(run(&compile(&typer(&rewriter(&parser(&tokenizer(&run_test)))))), stack);
}

#[derive(Debug, PartialEq)]
enum Token {
	Bracket(Bracket, Side),
	String(String),
	Identifier(String),
	Number(i64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Side {
	Open,
	Close,
}

impl Bracket {
	fn to_char(self, side: Side) -> char {
		match (self, side) {
			(Bracket::Round, Side::Open) => '(',
			(Bracket::Round, Side::Close) => ')',
			(Bracket::Curly, Side::Open) => '{',
			(Bracket::Curly, Side::Close) => '}',
			(Bracket::Square, Side::Open) => '[',
			(Bracket::Square, Side::Close) => ']',
		}
	}
}

fn tokenizer(chars: &[char]) -> Vec<Token> {
	let mut tokens: Vec<Token> = Vec::new();
	let mut i = 0;
	while i < chars.len() {
		match chars[i] {
			'#' => {
				while i < chars.len() && chars[i] != '\n' {
					i += 1;
				}
			}
			' ' | '\t' | '\n' | '\r' => {}
			'(' => tokens.push(Token::Bracket(Bracket::Round, Side::Open)),
			')' => tokens.push(Token::Bracket(Bracket::Round, Side::Close)),
			'{' => tokens.push(Token::Bracket(Bracket::Curly, Side::Open)),
			'}' => tokens.push(Token::Bracket(Bracket::Curly, Side::Close)),
			'[' => tokens.push(Token::Bracket(Bracket::Square, Side::Open)),
			']' => tokens.push(Token::Bracket(Bracket::Square, Side::Close)),
			'"' => {
				i += 1;
				let mut string = String::new();
				loop {
					if i >= chars.len() {
						panic!("missing end quote");
					} else if chars[i] == '\\' {
						i += 1;
						string.push(match chars.get(i) {
							Some('\\') => '\\',
							Some('t') => '\t',
							Some('n') => '\n',
							Some('r') => '\r',
							Some('"') => '"',
							Some(c) => panic!("invalid escape character {}", c),
							None => panic!("missing escape character"),
						});
					} else if chars[i] == '"' {
						break;
					} else {
						string.push(chars[i]);
					}
					i += 1;
				}
				tokens.push(Token::String(string));
			}
			c => {
				let start = i;
				let is_alphanumeric =
					|c: char| matches!(c, '_' | 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9');
				while i < chars.len()
					&& !"\"#(){}{} \t\n".contains(chars[i])
					&& is_alphanumeric(c) == is_alphanumeric(chars[i])
				{
					i += 1;
				}

				tokens.push(Token::Identifier(chars[start..i].iter().collect()));

				i -= 1;
			}
		}

		i += 1;
	}

	for token in &mut tokens {
		if let Token::Identifier(i) = token {
			let chars: Vec<char> = i.chars().collect();
			if '0' <= chars[0] && chars[0] <= '9' {
				if chars.len() == 1 {
					*token = Token::Number(chars[0] as i64 - '0' as i64);
				} else {
					let mut value = 0;
					let (base, start) = match (chars[0], chars[1]) {
						('0', 'x') => (16, 2),
						('0', 'b') => (2, 2),
						(_, _) => (10, 0),
					};
					if start >= chars.len() {
						panic!("no digits after base specifier")
					}
					for c in chars.iter().skip(start) {
						if *c != '_' {
							let digit = match *c {
								'0'..='9' => *c as i64 - '0' as i64,
								'A'..='F' => *c as i64 - 'A' as i64 + 10,
								'a'..='f' => *c as i64 - 'a' as i64 + 10,
								d => panic!("invalid digit {}", d),
							};
							if digit >= base {
								panic!("invalid digit {} in base {}", digit, base)
							}
							value = value * base + digit;
						}
					}
					*token = Token::Number(value);
				}
			}
		}
	}
	tokens
}

//                   name     func     left   right
type Operator<'a> = (&'a str, &'a str, usize, usize);
//                    operators  right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
	(&[("@", "nop", 1, 1)], false),
	(&[("-", "neg", 0, 1), ("not", "_not_", 0, 1)], true),
	(&[("*", "mul", 1, 1)], false),
	(&[("+", "add", 1, 1)], false),
	(
		&[
			("==", "eq", 1, 1),
			("!=", "ne", 1, 1),
			("<", "lt", 1, 1),
			(">", "gt", 1, 1),
			("<=", "le", 1, 1),
			(">=", "ge", 1, 1),
		],
		false,
	),
	(&[("and", "_and_", 1, 1), ("or", "_or_", 1, 1)], true),
	(&[("=", "=", 1, 1), (":=", ":=", 1, 1)], true),
	(&[("if", "_if_", 0, 2), ("while", "_while_", 0, 2)], true),
	(&[("else", "_else_", 1, 1)], true),
];

#[derive(Clone, Debug, PartialEq)]
enum Parsed {
	Brackets(Bracket, Vec<Parsed>),
	String(String),
	Identifier(String),
	Number(i64),
	Operator(String, Vec<Parsed>),
}

fn parser(tokens: &[Token]) -> Vec<Parsed> {
	fn parse(tokens: &[Token], i: &mut usize, mut searching: Option<Bracket>) -> Vec<Parsed> {
		let mut trees = Vec::new();
		while *i < tokens.len() {
			let token = &tokens[*i];
			*i += 1;
			trees.push(match token {
				Token::Bracket(b, Side::Open) => {
					Parsed::Brackets(*b, parse(tokens, i, Some(*b)))
				}
				Token::Bracket(b, Side::Close) => match (b, searching) {
					(b, Some(c)) if *b == c => {
						searching = None;
						break;
					}
					(b, _) => panic!("extra {}", b.to_char(Side::Close)),
				},
				Token::String(s) => Parsed::String(s.clone()),
				Token::Identifier(i) => Parsed::Identifier(i.clone()),
				Token::Number(n) => Parsed::Number(*n),
			});
		}
		if let Some(b) = searching {
			panic!("extra {}", b.to_char(Side::Open))
		}
		for (operators, right) in OPERATORS {
			let mut j = if *right { trees.len().wrapping_sub(1) } else { 0 };
			while let Some(tree) = trees.get(j) {
				if let Parsed::Identifier(i) = tree {
					let i = i.clone();
					if let Some(operator) = operators.iter().find(|op| op.0 == i) {
						if j < operator.2 || j + operator.3 >= trees.len() {
							panic!("not enough operator arguments for {}", i)
						}
						trees.remove(j);
						let cs: Vec<Parsed> =
							trees.drain(j - operator.2..j + operator.3).collect();
						j -= operator.2;
						trees.insert(j, Parsed::Operator(i, cs));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}
		trees
	}
	parse(tokens, &mut 0, None)
}

#[derive(Clone, Debug, PartialEq)]
enum Rewritten {
	Block(Vec<Rewritten>),
	Identifier(String),
	Number(i64),
	Variable(usize),
	Declaration(usize),
	Assignment(usize),
}

fn rewriter(trees: &[Parsed]) -> Rewritten {
	fn rewriter(trees: &[Parsed], vars: &mut Vec<String>) -> Vec<Rewritten> {
		let mut out = vec![];
		for tree in trees {
			match tree {
				Parsed::Brackets(Bracket::Round, cs) => out.extend(rewriter(cs, vars)),
				Parsed::Brackets(Bracket::Curly, cs) => {
					out.push(Rewritten::Block(rewriter(cs, vars)));
				}
				Parsed::Brackets(Bracket::Square, _cs) => todo!(),
				Parsed::String(_) => todo!(),
				Parsed::Identifier(i) => match vars.iter().rposition(|j| i == j) {
					Some(v) => out.push(Rewritten::Variable(v)),
					None => out.push(Rewritten::Identifier(i.clone())),
				},
				Parsed::Number(n) => out.push(Rewritten::Number(*n)),
				Parsed::Operator(i, cs) => match i.as_str() {
					":=" | "=" => match &cs[..] {
						[Parsed::Identifier(v), rhs] => {
							out.append(&mut rewriter(&[rhs.clone()], vars));
							if i == ":=" {
								vars.push(v.clone());
							}
							out.push(match i.as_str() {
								":=" => Rewritten::Declaration(
									vars.iter().rposition(|j| v == j).unwrap(),
								),
								"=" => Rewritten::Assignment(
									vars.iter().rposition(|j| v == j).unwrap(),
								),
								_ => unreachable!(),
							});
						}
						cs => panic!("expected one var name and one value, found {:?}", cs),
					},
					_ => {
						let operator = OPERATORS
							.iter()
							.find_map(|(ops, _)| ops.iter().find(|op| op.0 == i))
							.unwrap();
						let mut cs = cs.clone();
						cs.reverse();
						out.append(&mut rewriter(&cs, vars));
						out.push(Rewritten::Identifier(operator.1.to_string()));
						out.push(Rewritten::Identifier("call".to_string()));
					}
				},
			}
		}
		out
	}
	Rewritten::Block(rewriter(trees, &mut vec![]))
}

#[derive(Clone, Debug, PartialEq)]
enum Typed {
	Block(Vec<Typed>, Effect, Scope),
	Identifier(String, Effect),
	Number(i64),
	Variable(usize, Type),
	Assignment(usize, Type),
}

impl Typed {
	fn get_effect(&self) -> Effect {
		match self {
			Typed::Block(_, e, _) => Effect::function(e.inputs.clone(), e.outputs.clone()),
			Typed::Identifier(_, e) => e.clone(),
			Typed::Number(_) => Effect::literal(Int),
			Typed::Variable(_, t) => Effect::new(vec![], vec![t.clone()]),
			Typed::Assignment(_, t) => Effect::new(vec![t.clone()], vec![]),
		}
	}
}

use Type::*;
#[derive(Clone, Debug, PartialEq)]
enum Type {
	Int,
	Bool,
	Block(Effect),
	Variable(usize),
}

impl Type {
	fn size_of(&self) -> usize {
		match self {
			Int => 8,
			Bool => 1,
			Block(_) => 8, // label
			Variable(_) => unreachable!(),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
struct Effect {
	inputs: Vec<Type>,
	outputs: Vec<Type>,
}

impl Effect {
	fn new(inputs: Vec<Type>, outputs: Vec<Type>) -> Effect {
		Effect { inputs, outputs }
	}

	fn literal(t: Type) -> Effect {
		Effect { inputs: vec![], outputs: vec![t] }
	}

	fn function(inputs: Vec<Type>, outputs: Vec<Type>) -> Effect {
		Effect::literal(Block(Effect { inputs, outputs }))
	}

	fn compose(&mut self, other: &Effect, vars: &mut TypeVars) {
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		assert!(self
			.outputs
			.drain(i..)
			.zip(&other.inputs[j..])
			.all(|(a, b)| vars.equalize(&a, b)));
		self.inputs.extend(other.inputs[..j].iter().cloned());
		self.outputs.extend(other.outputs.iter().cloned());
	}
}

struct TypeVars(Vec<Option<Type>>);

impl TypeVars {
	fn new_var(self: &mut TypeVars) -> Type {
		self.0.push(None);
		Variable(self.0.len() - 1)
	}

	fn old_var(self: &TypeVars) -> Type {
		Variable(self.0.len() - 1)
	}

	fn set_var(self: &mut TypeVars, i: usize, a: &Type) -> bool {
		match &self.0[i] {
			Some(b) => self.equalize(a, &b.clone()),
			None => {
				self.0[i] = Some(a.clone());
				true
			}
		}
	}

	fn get_var(self: &TypeVars, i: usize) -> &Option<Type> {
		match &self.0[i] {
			Some(Variable(j)) => self.get_var(*j),
			o => o,
		}
	}

	fn equalize(&mut self, a: &Type, b: &Type) -> bool {
		match (a, b) {
			(Int, Int) | (Bool, Bool) => true,
			(Block(a), Block(b)) => {
				a.inputs.iter().zip(&b.inputs).all(|(a, b)| self.equalize(a, b))
					&& a.outputs.iter().zip(&b.outputs).all(|(a, b)| self.equalize(a, b))
			}
			(Variable(i), b) | (b, Variable(i)) => self.set_var(*i, b),
			_ => false,
		}
	}

	fn substitute_type(&self, t: &Type) -> Type {
		match t {
			Int => Int,
			Bool => Bool,
			Block(e) => Block(self.substitute_effect(e)),
			Variable(i) => self.get_var(*i).as_ref().expect("unsolved type var").clone(),
		}
	}

	fn substitute_effect(&self, e: &Effect) -> Effect {
		Effect {
			inputs: e.inputs.iter().map(|t| self.substitute_type(t)).collect(),
			outputs: e.outputs.iter().map(|t| self.substitute_type(t)).collect(),
		}
	}

	fn substitute(&self, typed: &mut Typed) {
		match typed {
			Typed::Block(_, e, _) | Typed::Identifier(_, e) => *e = self.substitute_effect(e),
			Typed::Number(_) => {}
			Typed::Variable(_, t) | Typed::Assignment(_, t) => *t = self.substitute_type(t),
		}
		if let Typed::Block(cs, _, scope) = typed {
			scope.borrow_mut().vars.values_mut().for_each(|t| *t = self.substitute_type(t));
			cs.iter_mut().for_each(|c| self.substitute(c));
		}
	}
}

use std::{cell::RefCell, collections::BTreeMap, rc::Rc}; // avoiding HashMap to avoid random order
type Scope = Rc<RefCell<S>>;
#[derive(Clone, Debug, PartialEq)]
struct S {
	vars: BTreeMap<usize, Type>,
	parent: Option<Scope>,
}

fn new_scope(parent: Option<Scope>) -> Scope {
	Rc::new(RefCell::new(S { vars: BTreeMap::new(), parent }))
}

fn set_var(scope: Scope, name: usize, value: Type) {
	find_scope(scope.clone(), name).unwrap_or(scope).borrow_mut().vars.insert(name, value);
}

fn get_var(scope: Scope, name: usize) -> Option<Type> {
	find_scope(scope.clone(), name).unwrap_or(scope).borrow().vars.get(&name).cloned()
}

fn find_scope(scope: Scope, name: usize) -> Option<Scope> {
	if scope.borrow().vars.contains_key(&name) {
		Some(scope)
	} else if let Some(parent) = &scope.borrow().parent {
		find_scope(parent.clone(), name)
	} else {
		None
	}
}

fn typer(tree: &Rewritten) -> Typed {
	fn typer(tree: &Rewritten, vars: &mut TypeVars, effect: &mut Effect, scope: Scope) -> Typed {
		let out = match tree {
			Rewritten::Block(cs) => {
				let mut e = Effect::new(vec![], vec![]);
				let s = new_scope(Some(scope));
				let cs = cs.iter().map(|c| typer(c, vars, &mut e, s.clone())).collect();
				Typed::Block(cs, e, s)
			}
			Rewritten::Identifier(i) => Typed::Identifier(
				i.clone(),
				match i.as_str() {
					"call" => {
						let call = |e: &Effect| {
							let mut i = e.inputs.clone();
							i.push(Block(Effect::new(e.inputs.clone(), e.outputs.clone())));
							Effect::new(i, e.outputs.clone())
						};
						match effect.outputs.last() {
							Some(Block(e)) => call(e),
							Some(Variable(v)) => match vars.get_var(*v) {
								Some(Block(e)) => call(e),
								t => panic!("call expected block, found {:?}", t),
							},
							t => panic!("call expected block, found {:?}", t),
						}
					}
					"nop" => Effect::new(vec![], vec![]),
					"false" | "true" => Effect::literal(Bool),
					"neg" => Effect::function(vec![Int], vec![Int]),
					"_not_" => Effect::function(vec![Bool], vec![Bool]),
					"mul" | "add" => Effect::function(vec![Int, Int], vec![Int]),
					"eq" | "ne" => {
						Effect::function(vec![vars.new_var(), vars.old_var()], vec![Bool])
					}
					"lt" | "gt" | "le" | "ge" => Effect::function(vec![Int, Int], vec![Bool]),
					"_and_" | "_or_" => Effect::function(vec![Bool, Bool], vec![Bool]),
					"_if_" | "_else_" => todo!("optionals"),
					"_while_" => Effect::function(
						vec![Block(Effect::literal(Bool)), Block(Effect::new(vec![], vec![]))],
						vec![],
					),
					i => todo!("could not type {:?}", i),
				},
			),
			Rewritten::Number(n) => Typed::Number(*n),
			Rewritten::Variable(v) => Typed::Variable(*v, get_var(scope, *v).unwrap()),
			Rewritten::Declaration(v) => {
				set_var(scope, *v, vars.new_var());
				Typed::Assignment(*v, vars.old_var()) // Typed::Block handles allocation
			}
			Rewritten::Assignment(v) => match get_var(scope, *v) {
				Some(t) => Typed::Assignment(*v, t),
				None => todo!("could not type variable"),
			},
		};
		effect.compose(&out.get_effect(), vars);
		out
	}
	let mut vars = TypeVars(vec![]);
	let mut effect = Effect::new(vec![], vec![]);
	let mut tree = typer(tree, &mut vars, &mut effect, new_scope(None));
	if !effect.inputs.is_empty() {
		panic!("program expected {:?}", effect.inputs);
	}
	vars.substitute(&mut tree);
	tree
}

use Operation::*;
#[derive(Clone, Debug, PartialEq)]
enum Operation {
	IntPush(i64),
	IntNeg,
	IntMul,
	IntAdd,
	IntEq,
	IntLt,
	IntGt,
	BoolPush(bool),
	BoolNot,
	BoolAnd,
	BoolOr,
	BoolEq,
	Alloc(usize /* size */),
	Free(usize /* size */),
	Move(usize /* start */, usize /* end */, usize /* size */), // indices from end of stack
	Copy(usize /* start */, usize /* end */, usize /* size */), // indices from end of stack
	Label(i64),
	Goto,
}

fn compile(tree: &Typed) -> Vec<Operation> {
	fn block(mut code: Vec<Operation>, rets_size: usize, labels: &mut i64) -> Vec<Operation> {
		let (start, end) = (*labels, *labels + 1);
		*labels += 2;
		code.splice(0..0, [IntPush(start), IntPush(end), Goto, Label(start)]);
		code.extend([Move(rets_size, 0, 8), Goto, Label(end)]);
		code
	}
	fn compile(
		tree: &Typed,
		labels: &mut i64,
		scope: &mut BTreeMap<usize, usize>, // from variable index to stack offset
	) -> Vec<Operation> {
		let out = match tree {
			Typed::Block(cs, e, typed_scope) => {
				let args_size = e.inputs.iter().map(Type::size_of).sum();
				let rets_size = e.outputs.iter().map(Type::size_of).sum();
				let vars_size = typed_scope.borrow().vars.values().map(Type::size_of).sum();

				let mut s = scope.clone();
				for v in s.values_mut() {
					*v += vars_size + 8;
				}
				let mut offset = 0;
				for (k, v) in &typed_scope.borrow().vars {
					s.insert(*k, offset + args_size);
					offset += v.size_of();
				}

				let mut code: Vec<Operation> =
					cs.iter().flat_map(|c| compile(c, labels, &mut s)).collect();
				code.splice(0..0, [Alloc(vars_size), Move(0, args_size, vars_size)]);
				code.extend([Move(rets_size, 0, vars_size), Free(vars_size)]);

				block(code, rets_size, labels)
			}
			Typed::Identifier(s, e) => match (s.as_str(), &e.inputs[..], &e.outputs[..]) {
				(s, [], [Block(Effect { inputs, outputs })]) => block(
					match (s, &inputs[..], &outputs[..]) {
						("neg", [Int], [Int]) => vec![IntNeg],
						("_not_", [Bool], [Bool]) => vec![BoolNot],
						("mul", [Int, Int], [Int]) => vec![IntMul],
						("add", [Int, Int], [Int]) => vec![IntAdd],
						("eq", [Int, Int], [Bool]) => vec![IntEq],
						("eq", [Bool, Bool], [Bool]) => vec![BoolEq],
						("ne", [Int, Int], [Bool]) => vec![IntEq, BoolNot],
						("ne", [Bool, Bool], [Bool]) => vec![BoolEq, BoolNot],
						("lt", [Int, Int], [Bool]) => vec![IntLt],
						("gt", [Int, Int], [Bool]) => vec![IntGt],
						("le", [Int, Int], [Bool]) => vec![IntGt, BoolNot],
						("ge", [Int, Int], [Bool]) => vec![IntLt, BoolNot],
						("_and_", [Bool, Bool], [Bool]) => vec![BoolAnd],
						("_or_", [Bool, Bool], [Bool]) => vec![BoolOr],
						(s, i, o) => todo!("could not compile {:?} {:?}->{:?}", s, i, o),
					},
					outputs.iter().map(Type::size_of).sum(),
					labels,
				),
				("call", inputs, _) => {
					let ret = *labels;
					*labels += 1;
					let args_size = inputs.iter().map(Type::size_of).sum();
					vec![IntPush(ret), Move(0, args_size, 8), Goto, Label(ret)]
				}
				("false", [], [Bool]) => vec![BoolPush(false)],
				("true", [], [Bool]) => vec![BoolPush(true)],
				("nop", [], []) => vec![],
				(s, i, o) => todo!("could not compile {:?} {:?}->{:?}", s, i, o),
			},
			Typed::Number(n) => vec![IntPush(*n)],
			Typed::Variable(v, t) => vec![Copy(*scope.get(v).unwrap(), 0, t.size_of())],
			Typed::Assignment(v, t) => {
				let (ptr, size) = (*scope.get(v).unwrap(), t.size_of());
				vec![Move(ptr, 0, size), Free(size), Move(0, ptr - size, size)]
			}
		};
		let e = tree.get_effect();
		for v in scope.values_mut() {
			*v -= e.inputs.iter().map(Type::size_of).sum::<usize>();
			*v += e.outputs.iter().map(Type::size_of).sum::<usize>();
		}
		out
	}
	compile(tree, &mut 0, &mut BTreeMap::new())
}

#[derive(Clone, Debug, PartialEq)]
struct Stack(Vec<u8>);

impl Stack {
	fn new() -> Stack {
		Stack(vec![])
	}

	fn push<A>(&mut self, value: A) {
		unsafe {
			self.0.extend(std::slice::from_raw_parts(
				&value as *const A as *const u8,
				std::mem::size_of::<A>(),
			))
		}
	}

	fn pop<A: Clone>(&mut self) -> A {
		unsafe {
			let data = self.0.split_off(self.0.len() - std::mem::size_of::<A>());
			(*data.as_ptr().cast::<A>()).clone()
		}
	}

	fn binary<A: Clone, B, F: Fn(A, A) -> B>(&mut self, f: F) {
		let (a, b) = (self.pop(), self.pop());
		self.push(f(a, b));
	}

	fn unary<A: Clone, B, F: Fn(A) -> B>(&mut self, f: F) {
		let a = self.pop();
		self.push(f(a));
	}
}

fn run(code: &[Operation]) -> Stack {
	let mut code = code.to_vec();
	code.extend([IntPush(-1), Move(0, 8, 8), Goto, Label(-1)]);
	// for (i, o) in code.iter().enumerate() {
	// 	println!("{}: {:?}", i, o);
	// }

	let mut stack = Stack::new();
	let mut i = 0;
	while i < code.len() {
		// println!("{:?}\n{}", stack, i);
		match code[i] {
			IntPush(i) => stack.push(i),
			IntMul => stack.binary(|a: i64, b| a * b),
			IntAdd => stack.binary(|a: i64, b| a + b),
			IntNeg => stack.unary(|a: i64| -a),
			IntEq => stack.binary(|a: i64, b| a == b),
			IntLt => stack.binary(|a: i64, b| a < b),
			IntGt => stack.binary(|a: i64, b| a > b),
			BoolPush(b) => stack.push(b),
			BoolNot => stack.unary(|a: bool| !a),
			BoolAnd => stack.binary(|a: bool, b| a & b),
			BoolOr => stack.binary(|a: bool, b| a | b),
			BoolEq => stack.binary(|a: bool, b| a == b),
			Alloc(size) => stack.0.extend(std::iter::repeat(0u8).take(size)),
			Free(size) => stack.0.truncate(stack.0.len() - size),
			Move(start, end, size) => {
				let l = stack.0.len() - size;
				let data = stack.0.drain(l - start..l - start + size).collect::<Vec<u8>>();
				stack.0.splice(l - end..l - end, data);
			}
			Copy(start, end, size) => {
				let l = stack.0.len();
				let data = stack.0[l - start - size..l - start].to_vec();
				stack.0.splice(l - end..l - end, data);
			}
			Label(_) => {}
			Goto => {
				let a = stack.pop::<i64>();
				i = code
					.iter()
					.position(|o| matches!(o, Label(b) if a == *b))
					.unwrap_or_else(|| panic!("could not find label {}", a))
			}
		}
		i += 1;
	}
	stack
}
