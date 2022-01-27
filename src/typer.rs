use crate::parser::AST;
use crate::tokenizer::Token;

pub enum TypedAST {
	Token(Token, Type),
	Call(Box<TypedAST>, Box<TypedAST>, Type),
	List(Token, Vec<TypedAST>, Token, Type),
}

impl TypedAST {
	fn get_type(&self) -> &Type {
		match self {
			TypedAST::Token(_, t) => t,
			TypedAST::Call(_, _, t) => t,
			TypedAST::List(_, _, _, t) => t,
		}
	}
}

impl std::fmt::Debug for TypedAST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TypedAST::Token(a, t) => write!(f, "{:?}:{:?}", a, t),
			TypedAST::Call(a, b, t) => write!(f, "<{:?} {:?}>:{:?}", a, b, t),
			TypedAST::List(a, xs, b, t) => {
				write!(
					f,
					"{a:?}{}{b:?}:{t:?}",
					xs.iter().map(|x| format!("{x:?}")).collect::<Vec<String>>().join(" "),
				)
			}
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
	Bool,
	Int,
	Unit,
	Array(Box<Type>),
	Func(Box<Type>, Box<Type>),
}

pub fn annotate(ast: &AST) -> TypedAST {
	fn unary_func(a: Type, b: Type) -> Type {
		Type::Func(Box::new(a), Box::new(b))
	}
	fn binary_func(a: Type, b: Type, c: Type) -> Type {
		Type::Func(Box::new(a), Box::new(Type::Func(Box::new(b), Box::new(c))))
	}
	match ast {
		AST::Token(t) => TypedAST::Token(
			t.clone(),
			match &*t.string {
				"true" | "false" => Type::Bool,
				s if s.chars().next().unwrap().is_numeric() => Type::Int,
				"add_int" => binary_func(Type::Int, Type::Int, Type::Int),
				"sub_int" => binary_func(Type::Int, Type::Int, Type::Int),
				"mul_int" => binary_func(Type::Int, Type::Int, Type::Int),
				"neg_int" => unary_func(Type::Int, Type::Int),
				"if_" => todo!("if statements"),
				"else_" => todo!("else statements"),
				s => todo!("{:?}", s),
			},
		),
		AST::Call(f, x) => {
			let f = annotate(f);
			let x = annotate(x);
			if let Type::Func(a, b) = f.get_type().clone() {
				assert!(*a == *x.get_type());
				TypedAST::Call(Box::new(f), Box::new(x), *b.clone())
			} else {
				panic!("expected function type: {:?}", f);
			}
		}
		AST::List(a, xs, b) => {
			let xs: Vec<TypedAST> = xs.iter().map(annotate).collect();
			let t = match &*a.string {
				"(" => {
					assert!(xs.len() == 1);
					xs[0].get_type().clone()
				}
				"{" => match xs.last() {
					Some(x) => x.get_type().clone(),
					None => Type::Unit,
				},
				"[" => match xs.get(0).map(|x| x.get_type().clone()) {
					Some(t) => {
						assert!(xs.iter().all(|x| *x.get_type() == t));
						Type::Array(Box::new(t))
					}
					None => todo!(),
				},
				_ => todo!(),
			};
			TypedAST::List(a.clone(), xs, b.clone(), t)
		}
	}
}
