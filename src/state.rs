use crate::*;

/// The state of the compiler.
pub struct State {
    /// The source files that are being processed.
    pub sources: Store<Source>,
    /// The tree that represents the program.
    pub nodes: Store<Node>,
}

impl Default for State {
    fn default() -> Self {
        let sources = Store::default();

        let mut nodes = Store::default();
        assert_eq!(ROOT, nodes.push(Node::root()));

        State { sources, nodes }
    }
}

/// One node in the tree that represents the program
/// Each node maintains a doubly linked list of its children.
/// INVARIANT: each node except the root is always referred to twice.
pub struct Node {
    /// The type of this node.
    pub tag: Tag,
    /// Where this node originated
    pub span: Span,
    /// The next sibling of this node.
    pub next: OptionIndex,
    /// The previous sibling of this node.
    pub prev: OptionIndex,
    /// The first child of this node.
    pub head: OptionIndex,
    /// The last child of this node.
    pub last: OptionIndex,
}

/// The index of the root node.
pub const ROOT: usize = 0;

impl Node {
    fn root() -> Self {
        Node {
            tag: Tag::Root,
            span: Span {
                source: 0,
                lo: 0,
                hi: 0,
            },
            next: OptionIndex::NONE,
            prev: OptionIndex::NONE,
            head: OptionIndex::NONE,
            last: OptionIndex::NONE,
        }
    }
}

impl State {
    /// Create a new node with the given data as a child of some parent node.
    pub fn push_child(&mut self, parent: usize, tag: Tag, span: Span) -> usize {
        let child = self.nodes.push(Node {
            tag,
            span,
            next: OptionIndex::NONE,
            prev: self.nodes[parent].last,
            head: OptionIndex::NONE,
            last: OptionIndex::NONE,
        });
        if let Some(prev) = self.nodes[parent].last.unpack() {
            self.nodes[prev].next = OptionIndex::some(child);
            self.nodes[parent].last = OptionIndex::some(child);
        } else {
            self.nodes[parent].head = OptionIndex::some(child);
            self.nodes[parent].last = OptionIndex::some(child);
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
