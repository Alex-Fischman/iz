use crate::Tree;
use std::collections::VecDeque;

#[derive(Default)]
pub struct Passes(VecDeque<Pass>);
type Pass = Box<dyn Fn(&mut Tree)>;

impl Passes {
    pub fn push(&mut self, f: impl Fn(&mut Tree) + 'static) {
        self.0.push_back(Box::new(f));
    }
}

impl Tree {
    /// Requires `self` to contain a `Passes` object.
    pub fn run(&mut self) {
        while let Some(pass) = self.get_mut::<Passes>().unwrap().0.pop_front() {
            pass(self)
        }
    }
}
