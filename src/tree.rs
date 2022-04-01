#[derive(Clone, Debug)]
pub enum Tree<Item, List, Node> {
	Leaf(Item, Node),
	List(List, Vec<Tree<Item, List, Node>>, Node),
	Call(Box<Tree<Item, List, Node>>, Box<Tree<Item, List, Node>>, Node),
}

impl<I, L, N> Tree<I, L, N> {
	pub fn walk<Leaf, List, Call, R>(
		&self,
		leaf: &mut Leaf,
		list: &mut List,
		call: &mut Call,
	) -> R
	where
		Leaf: FnMut(&I, &N) -> R,
		List: FnMut(&L, Vec<R>, &N) -> R,
		Call: FnMut(R, R, &N) -> R,
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
}
