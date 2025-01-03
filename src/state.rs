use crate::*;

/// The state of the compiler.
pub struct State {
    /// The source files that are being processed.
    pub sources: Vec<Source>,
    /// The tree that represents the program.
    pub nodes: Vec<Node>,
    /// The span for each node in the program.
    pub spans: Vec<Span>,
}

/// The index of the root node.
pub const ROOT: usize = 0;

impl Default for State {
    fn default() -> Self {
        State {
            sources: Vec::new(),
            nodes: vec![Node {
                tag: Tag::Root,
                next: NodeRef::NONE,
                prev: NodeRef::NONE,
                head: NodeRef::NONE,
                last: NodeRef::NONE,
            }],
            spans: vec![Span {
                source: usize::MAX,
                lo: usize::MAX,
                hi: usize::MAX,
            }],
        }
    }
}

/// One node in the tree that represents the program
/// Each node maintains a doubly linked list of its children.
/// INVARIANT: each node except the root is always referred to twice.
pub struct Node {
    /// The type of this node.
    pub tag: Tag,
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
    /// Create a new node with the given data as a child of some parent node.
    pub fn push_child(&mut self, parent: usize, tag: Tag, span: Span) -> usize {
        let child = self.nodes.len();
        self.nodes.push(Node {
            tag,
            next: NodeRef::NONE,
            prev: self.nodes[parent].last,
            head: NodeRef::NONE,
            last: NodeRef::NONE,
        });
        self.spans.push(span);

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

/// The type of a node.
pub enum Tag {
    /// The root node.
    Root,
    /// An open bracket, annotated with the closing string.
    /// For example, `"{" : Opener("}")`.
    Opener(&'static str),
    /// A close bracket.
    Closer,
    /// A string, with its escaped contents.
    /// For example, `"\\" : String("\")`
    String(String),
    /// An identifer.
    Identifier,
}
