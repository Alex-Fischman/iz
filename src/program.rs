use crate::*;

/// A custom `Iterator` trait that supports returning `Result`s.
pub trait Program {
    /// The type of one element of the program.
    type Item;

    /// Get the next element of the program.
    /// If the program represents a tree, this is always a postorder traversal.
    fn next(&mut self) -> Result<Option<Self::Item>>;

    /// Transform this program into a different program.
    fn map<Y>(self, f: impl FnMut(Self::Item) -> Y) -> impl Program<Item = Y>
    where
        Self: Sized,
    {
        Map(self, f)
    }

    /// Transform this program into a `Vec`.
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

impl<X, Y, P, F> Program for Map<P, F>
where
    P: Program<Item = X>,
    F: FnMut(X) -> Y,
{
    type Item = Y;

    fn next(&mut self) -> Result<Option<Y>> {
        self.0.next().map(|option| option.map(|x| self.1(x)))
    }
}
