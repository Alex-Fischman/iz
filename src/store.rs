use crate::*;
use std::marker::PhantomData;

/// An opaque reference to a value in a `Store<T>`.
pub struct Index<T>(usize, PhantomData<T>);

const _: () = {
    use std::mem::size_of;
    assert!(size_of::<Index<()>>() == size_of::<usize>());
};

impl<T> Index<T> {
    /// Create a new `Index`.
    /// This has to be `pub`, but it should be used sparingly.
    #[must_use]
    pub const fn new(i: usize) -> Self {
        Index(i, PhantomData)
    }
}

// have to manually implement traits to avoid type restriction on `T`
impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Index<T> {}
impl<T> Debug for Index<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Index<T>) -> bool {
        self.0 == other.0
    }
}

/// A `Vec` of values, indexed by `Index<T>`s.
#[derive(Clone)]
pub struct Store<T>(Vec<T>);

impl<T> Store<T> {
    /// Add a new element to the end.
    pub fn push(&mut self, x: T) -> Index<T> {
        self.0.push(x);
        Index::new(self.0.len() - 1)
    }
}

impl<T> Default for Store<T> {
    fn default() -> Self {
        Store(Vec::new())
    }
}

impl<T> std::ops::Index<Index<T>> for Store<T> {
    type Output = T;
    fn index(&self, i: Index<T>) -> &T {
        &self.0[i.0]
    }
}

impl<T> std::ops::IndexMut<Index<T>> for Store<T> {
    fn index_mut(&mut self, i: Index<T>) -> &mut T {
        &mut self.0[i.0]
    }
}
