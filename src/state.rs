use crate::*;

/// The state of the compiler.
pub struct State {
    /// The source files that are being processed.
    pub sources: Store<Source>,
    /// The list of tags that nodes can have.
    pub tags: Store<Tag>,
    /// The tree that represents the program.
    pub nodes: Store<Node>,
}

impl Default for State {
    fn default() -> Self {
        let sources = Store::default();

        let mut tags = Store::default();
        assert_eq!(TAG_UNKNOWN, tags.push(Tag("unknown")));
        assert_eq!(TAG_ROOT, tags.push(Tag("root")));

        let mut nodes = Store::default();
        assert_eq!(ROOT, nodes.push(Node::root()));

        State {
            sources,
            tags,
            nodes,
        }
    }
}

/// The type of a node.
pub struct Tag(&'static str);

/// The default type to initialize nodes with.
pub const TAG_UNKNOWN: Index<Tag> = Index::new(0);
/// The type of the root node.
/// INVARIANT: only the `ROOT` node should have this tag.
pub const TAG_ROOT: Index<Tag> = Index::new(1);

impl Display for Tag {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// One node in the tree that represents the program
/// Each node maintains a doubly linked list of its children.
/// INVARIANT: each node except the root is always referred to twice.
pub struct Node {
    /// The type of this node.
    pub tag: Index<Tag>,
    /// Where this node originated
    pub span: Span,
    /// The next sibling of this node.
    pub next: OptionIndex<Node>,
    /// The previous sibling of this node.
    pub prev: OptionIndex<Node>,
    /// The first child of this node.
    pub head: OptionIndex<Node>,
    /// The last child of this node.
    pub last: OptionIndex<Node>,
}

/// The index of the root node.
pub const ROOT: Index<Node> = Index::new(0);

const ROOT_SPAN: Span = Span {
    source: Index::new(0),
    lo: 0,
    hi: 0,
};
impl Node {
    fn root() -> Self {
        Node {
            tag: TAG_ROOT,
            span: ROOT_SPAN,
            next: OptionIndex::NONE,
            prev: OptionIndex::NONE,
            head: OptionIndex::NONE,
            last: OptionIndex::NONE,
        }
    }
}

impl Store<Node> {
    /// Create a new node with the given data as a child of some parent node.
    pub fn push_child(&mut self, parent: Index<Node>, tag: Index<Tag>, span: Span) -> Index<Node> {
        let child = self.push(Node {
            tag,
            span,
            next: OptionIndex::NONE,
            prev: self[parent].last,
            head: OptionIndex::NONE,
            last: OptionIndex::NONE,
        });
        if let Some(prev) = self[parent].last.unpack() {
            self[prev].next = OptionIndex::some(child);
            self[parent].last = OptionIndex::some(child);
        } else {
            self[parent].head = OptionIndex::some(child);
            self[parent].last = OptionIndex::some(child);
        }
        child
    }
}
