use crate::expr::Expr;
use crate::token::Token;

#[derive(Clone)]
pub struct Env {
	head: Option<std::rc::Rc<std::cell::RefCell<Node>>>,
}

impl Env {
	pub fn new() -> Env {
		Env { head: None }
	}

	pub fn get_var(&self, s: &str) -> Option<Expr> {
		self.head.as_ref().and_then(|n| n.borrow().get_var(s))
	}

	pub fn add_var(&mut self, lhs: Token, rhs: Expr) {
		if let Some(h) = &self.head {
			if h.borrow_mut().add_var(lhs.clone(), rhs.clone()) {
				self.push_var(lhs, rhs)
			}
		} else {
			self.push_var(lhs, rhs)
		}
	}

	pub fn push_var(&mut self, lhs: Token, rhs: Expr) {
		self.head = Some(std::rc::Rc::new(std::cell::RefCell::new(Node {
			next: self.head.clone(),
			lhs,
			rhs,
		})))
	}
}

impl std::fmt::Debug for Env {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match &self.head {
			Some(n) => write!(f, "{:?}", n.borrow()),
			None => writeln!(f, "empty env"),
		}
	}
}

struct Node {
	next: Option<std::rc::Rc<std::cell::RefCell<Node>>>,
	lhs: Token,
	rhs: Expr,
}

impl Node {
	fn get_var(&self, s: &str) -> Option<Expr> {
		if self.lhs.string == s {
			Some(self.rhs.clone())
		} else {
			self.next.as_ref().and_then(|n| n.borrow().get_var(s))
		}
	}

	fn add_var(&mut self, lhs: Token, rhs: Expr) -> bool {
		if lhs.string == self.lhs.string {
			self.lhs = lhs;
			self.rhs = rhs;
			false
		} else if let Some(n) = &self.next {
			n.borrow_mut().add_var(lhs, rhs)
		} else {
			true
		}
	}
}

impl std::fmt::Debug for Node {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		if let Some(n) = &self.next {
			write!(f, "{:?}", n.borrow())?;
		}
		if let Some(p) = &self.lhs.pos {
			write!(f, "{:?} \t", p)?;
		}
		writeln!(f, "{:?}: {:?}", self.lhs, self.rhs)
	}
}
