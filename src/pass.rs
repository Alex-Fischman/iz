use crate::Tree;
use std::collections::VecDeque;

#[derive(Default)]
pub struct Passes(VecDeque<Pass>);
type Pass = Box<dyn Fn(&mut Tree)>;

impl Passes {
    pub fn push(&mut self, f: impl Fn(&mut Tree) + 'static) {
        self.0.push_back(Box::new(f));
    }

    pub fn run(self, tree: &mut Tree) {
        tree.insert(self);
        while let Some(pass) = tree.get_mut::<Passes>().unwrap().0.pop_front() {
            pass(tree)
        }
    }
}
