pub struct Tree<T> {
    pub value: T,
    pub children: Vec<Tree<T>>,
}

impl<T> Tree<T> {
    pub fn new(value: T) -> Tree<T> {
        Tree {
            value: value,
            children: Vec::new(),
        }
    }

    pub fn for_each<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut Tree<T>),
    {
        self.children.iter_mut().for_each(|c| c.for_each(f));
        f(self);
    }
}

impl<T: Clone> Tree<T> {
    pub fn map<F, S>(&self, f: F) -> Tree<S>
    where
        F: Fn(T) -> S + Copy,
    {
        Tree {
            value: f(self.value.clone()),
            children: self.children.iter().map(|t| t.map(f)).collect(),
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn internal<T: std::fmt::Debug>(
            f: &mut std::fmt::Formatter,
            t: &Tree<T>,
            depth: usize,
        ) -> std::fmt::Result {
            writeln!(f, "{}{:?}", "\t".repeat(depth), t.value)?;
            for c in &t.children {
                internal(f, &c, depth + 1)?
            }
            Ok(())
        }

        internal(f, self, 0)
    }
}
