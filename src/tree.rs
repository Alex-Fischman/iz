#[derive(Clone)]
pub enum Tree<Item, List, Node> {
	Leaf(Item, Node),
	List(List, Vec<Tree<Item, List, Node>>, Node),
	Call(Box<Tree<Item, List, Node>>, Box<Tree<Item, List, Node>>, Node),
}

impl<Item: PartialEq, List: PartialEq, Node: PartialEq> PartialEq for Tree<Item, List, Node> {
	fn eq(&self, other: &Self) -> bool {
		Tree::compare(self, other, PartialEq::eq, PartialEq::eq, PartialEq::eq)
	}
}

use std::fmt::Debug;
impl<I: Debug, L: Debug, N> Debug for Tree<I, L, N> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tree::Leaf(i, _) => write!(f, "({:?})", i),
			Tree::List(l, xs, _) => write!(f, "{:?}{:#?}", l, xs),
			Tree::Call(a, b, _) => write!(f, "( {:?} @ {:?} )", **a, **b),
		}
	}
}

impl<I, L, N> Tree<I, L, N> {
	pub fn call(f: Self, x: Self, n: N) -> Self {
		Tree::Call(Box::new(f), Box::new(x), n)
	}

	pub fn walk<A, B, C, R>(&self, leaf: &mut A, list: &mut B, call: &mut C) -> R
	where
		A: FnMut(&I, &N) -> R,
		B: FnMut(&L, Vec<R>, &N) -> R,
		C: FnMut(R, R, &N) -> R,
	{
		match self {
			Tree::Leaf(i, n) => leaf(i, n),
			Tree::List(l, v, n) => {
				let v = v.iter().map(|t| t.walk(leaf, list, call)).collect();
				list(l, v, n)
			}
			Tree::Call(a, b, n) => {
				let a = a.walk(leaf, list, call);
				let b = b.walk(leaf, list, call);
				call(a, b, n)
			}
		}
	}

	pub fn compare<A, B, C>(a: &Self, b: &Self, item: A, list: B, node: C) -> bool
	where
		A: Fn(&I, &I) -> bool + Copy,
		B: Fn(&L, &L) -> bool + Copy,
		C: Fn(&N, &N) -> bool + Copy,
	{
		match (a, b) {
			(Tree::Leaf(i, n), Tree::Leaf(j, o)) => item(i, j) && node(n, o),
			(Tree::List(l, v, n), Tree::List(m, w, o)) => {
				list(l, m)
					&& v.iter().zip(w).all(|(a, b)| Tree::compare(a, b, item, list, node))
					&& node(n, o)
			}
			(Tree::Call(f, x, n), Tree::Call(g, y, o)) => {
				Tree::compare(f, g, item, list, node)
					&& Tree::compare(x, y, item, list, node)
					&& node(n, o)
			}
			_ => false,
		}
	}
}
