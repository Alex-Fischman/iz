use crate::*;

/// A custom `Iterator` trait that supports returning `Result`s.
pub trait Iterator {
    /// The type of one element.
    type Item;

    /// Get the next element of the iterator.
    /// If we're iterating over a tree, this should be a postorder traversal.
    fn next(&mut self) -> Result<Option<Self::Item>>;

    /// Transform each element of this iterator using a function.
    fn map<Y>(self, f: impl FnMut(Self::Item) -> Y) -> impl Iterator<Item = Y>
    where
        Self: Sized,
    {
        Map(self, f)
    }

    /// Transform this iterator into a `Vec`.
    /// Useful for testing and at the end of compilation.
    fn collect(mut self) -> Result<Vec<Self::Item>>
    where
        Self: Sized,
    {
        let mut xs = Vec::new();
        while let Some(x) = self.next()? {
            xs.push(x);
        }
        Ok(xs)
    }
}

struct Map<A, B>(A, B);

impl<X, Y, P, F> Iterator for Map<P, F>
where
    P: Iterator<Item = X>,
    F: FnMut(X) -> Y,
{
    type Item = Y;

    fn next(&mut self) -> Result<Option<Y>> {
        self.0.next().map(|option| option.map(|x| self.1(x)))
    }
}
