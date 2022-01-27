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

use Type::*;
#[derive(Clone, Debug)]
pub enum Type {
	Bool,
	Int,
	Unit,
	Opt(Box<Type>),
	Array(Box<Type>),
	Func(Box<Type>, Box<Type>),
	Var(usize),
}

impl Type {
	fn count_vars(&self) -> usize {
		match self {
			Bool | Int | Unit => 0,
			Opt(t) | Array(t) => t.count_vars(),
			Func(t, u) => t.count_vars().max(u.count_vars()),
			Var(i) => *i + 1,
		}
	}
}

type Vars = [Option<Type>];
impl PartialEq for Type {
	fn eq(&self, other: &Type) -> bool {
		fn eq(a: &Type, b: &Type, a_vs: &mut Vars, b_vs: &mut Vars) -> bool {
			match (a, b) {
				(Bool, Bool) => true,
				(Int, Int) => true,
				(Unit, Unit) => true,
				(Opt(a), Opt(b)) => eq(a, b, a_vs, b_vs),
				(Array(a), Array(b)) => eq(a, b, a_vs, b_vs),
				(Func(a, x), Func(b, y)) => eq(a, b, a_vs, b_vs) && eq(x, y, a_vs, b_vs),
				(Var(i), Var(j)) => match (&a_vs[*i], &b_vs[*j]) {
					(Some(t), Some(u)) => eq(&t.clone(), &u.clone(), a_vs, b_vs),
					(Some(t), None) => {
						b_vs[*j] = Some(t.clone());
						true
					}
					(None, Some(t)) => {
						a_vs[*i] = Some(t.clone());
						true
					}
					(None, None) => todo!(),
				},
				(Var(i), b) => match &a_vs[*i] {
					Some(a) => eq(&a.clone(), b, a_vs, b_vs),
					None => {
						a_vs[*i] = Some(b.clone());
						true
					}
				},
				(a, Var(j)) => match &b_vs[*j] {
					Some(b) => eq(a, &b.clone(), a_vs, b_vs),
					None => {
						b_vs[*j] = Some(a.clone());
						true
					}
				},
				_ => false,
			}
		}
		eq(
			self,
			other,
			&mut vec![None; self.count_vars()][..],
			&mut vec![None; other.count_vars()][..],
		)
	}
}

pub fn annotate(ast: &AST) -> TypedAST {
	fn binary_func(a: Type, b: Type, c: Type) -> Type {
		Func(Box::new(a), Box::new(Func(Box::new(b), Box::new(c))))
	}
	match ast {
		AST::Token(t) => TypedAST::Token(
			t.clone(),
			match &*t.string {
				"true" | "false" => Bool,
				s if s.chars().next().unwrap().is_numeric() => Int,
				"add_int" => binary_func(Int, Int, Int),
				"sub_int" => binary_func(Int, Int, Int),
				"mul_int" => binary_func(Int, Int, Int),
				"neg_int" => Func(Box::new(Int), Box::new(Int)),
				"if_" => binary_func(Bool, Var(0), Opt(Box::new(Var(0)))),
				"else_" => binary_func(Opt(Box::new(Var(0))), Var(0), Var(0)),
				s => todo!("{:?}", s),
			},
		),
		AST::Call(f, x) => {
			let f = annotate(f);
			let x = annotate(x);
			if let Func(a, b) = f.get_type().clone() {
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
					None => Unit,
				},
				"[" => match xs.get(0).map(|x| x.get_type().clone()) {
					Some(t) => {
						assert!(xs.iter().all(|x| *x.get_type() == t));
						Array(Box::new(t))
					}
					None => todo!(),
				},
				_ => todo!(),
			};
			TypedAST::List(a.clone(), xs, b.clone(), t)
		}
	}
}
