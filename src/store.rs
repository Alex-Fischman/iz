/// A packed representation of an `Option<usize>`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct OptionIndex(usize);

const _: () = {
    use std::mem::size_of;
    assert!(size_of::<OptionIndex>() == size_of::<usize>());
};

impl OptionIndex {
    /// Analog of `Option::None`.
    pub const NONE: Self = OptionIndex(usize::MAX);

    /// Analog of `Option::Some`.
    /// # Panics
    /// Will panic if the provided `Index` is `usize::MAX`.
    #[must_use]
    pub fn some(x: usize) -> Self {
        assert_ne!(x, usize::MAX);
        OptionIndex(x)
    }

    /// Convert from an unpacked `Option` to a packed `OptionIndex`.
    #[must_use]
    pub fn pack(x: Option<usize>) -> Self {
        if let Some(x) = x {
            Self::some(x)
        } else {
            Self::NONE
        }
    }

    /// Convert from a packed `OptionIndex` to an unpacked `Option`.
    #[must_use]
    pub fn unpack(self) -> Option<usize> {
        if self == Self::NONE {
            None
        } else {
            Some(self.0)
        }
    }
}

/// A `Vec` wrapper.
#[derive(Clone)]
pub struct Store<T>(Vec<T>);

impl<T> Store<T> {
    /// Add a new element to the end.
    pub fn push(&mut self, x: T) -> usize {
        self.0.push(x);
        self.0.len() - 1
    }
}

impl<T> Default for Store<T> {
    fn default() -> Self {
        Store(Vec::new())
    }
}

impl<T> std::ops::Index<usize> for Store<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        &self.0[i]
    }
}

impl<T> std::ops::IndexMut<usize> for Store<T> {
    fn index_mut(&mut self, i: usize) -> &mut T {
        &mut self.0[i]
    }
}
