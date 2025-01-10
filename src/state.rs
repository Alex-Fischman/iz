use crate::*;
use std::any::{Any, TypeId};

/// The state of the compiler.
pub struct State {
    /// The source files that are being processed.
    pub sources: Vec<Source>,
    /// The tree that represents the program.
    pub nodes: Vec<Node>,
    /// The map from tag names to tags.
    pub tags: HashMap<String, usize>,
    /// The map from tags to the type of the data
    /// stored by nodes with that tag.
    pub types: Vec<TypeId>,
}

/// The index of the root node.
pub const ROOT: usize = 0;

impl Default for State {
    fn default() -> Self {
        let mut out = State {
            sources: Vec::default(),
            nodes: Vec::default(),
            tags: HashMap::default(),
            types: Vec::default(),
        };

        let root = out.push_tag::<()>("Root");
        out.nodes.push(Node {
            tag: root,
            span: Span {
                source: usize::MAX,
                lo: usize::MAX,
                hi: usize::MAX,
            },
            data: Box::new(()),
            next: NodeRef::NONE,
            prev: NodeRef::NONE,
            head: NodeRef::NONE,
            last: NodeRef::NONE,
        });

        out
    }
}

impl State {
    /// Create a new tag.
    /// # Panics
    /// Will panic if the name is already in use.
    pub fn push_tag<T: 'static>(&mut self, name: &str) -> usize {
        let tag = self.tags.len();
        let old = self.tags.insert(name.to_owned(), tag);
        assert_eq!(old, None);
        self.types.push(TypeId::of::<T>());
        tag
    }
}

/// One node in the tree that represents the program
/// Each node maintains a doubly linked list of its children.
/// INVARIANT: each node except the root is always referred to twice.
pub struct Node {
    /// The type of this node.
    pub tag: usize,
    /// The span for this node.
    pub span: Span,
    /// The data for this node.
    pub data: Box<dyn Any>,
    /// The next sibling of this node.
    pub next: NodeRef,
    /// The previous sibling of this node.
    pub prev: NodeRef,
    /// The first child of this node.
    pub head: NodeRef,
    /// The last child of this node.
    pub last: NodeRef,
}

/// A representation of an `Option<NonZeroUsize>`.
/// This takes advantage of the fact that nodes never point to the root.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeRef(usize);

impl NodeRef {
    /// Analog of `Option::None`.
    pub const NONE: Self = NodeRef(ROOT);

    /// Analog of `Option::Some`.
    /// # Panics
    /// Will panic if the provided `Index` is `ROOT`.
    #[must_use]
    pub fn some(x: usize) -> Self {
        assert_ne!(x, ROOT);
        NodeRef(x)
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

impl State {
    /// Create a new node as a child of some parent node.
    pub fn push_child(
        &mut self,
        parent: usize,
        tag: usize,
        span: Span,
        data: Box<dyn Any>,
    ) -> usize {
        let child = self.nodes.len();
        self.nodes.push(Node {
            tag,
            span,
            data,
            next: NodeRef::NONE,
            prev: self.nodes[parent].last,
            head: NodeRef::NONE,
            last: NodeRef::NONE,
        });

        if let Some(prev) = self.nodes[parent].last.unpack() {
            self.nodes[prev].next = NodeRef::some(child);
            self.nodes[parent].last = NodeRef::some(child);
        } else {
            self.nodes[parent].head = NodeRef::some(child);
            self.nodes[parent].last = NodeRef::some(child);
        }

        child
    }
}
