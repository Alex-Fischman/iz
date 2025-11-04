use crate::*;

/// The state of the compiler.
pub struct State {
    sources: Vec<Source>,
    nodes: Vec<Node>,
    tables: Vec<Box<dyn Any>>,
}

impl State {
    /// The initial node for a new `State`.
    pub const ROOT: NodeId = NodeId(0);

    /// Create a new `State`.
    /// The provided source will have the returned `SourceId`.
    /// The new state will contain one empty node, `State::ROOT`.
    #[must_use]
    pub fn new(source: Source) -> (State, SourceId) {
        let mut state = State {
            sources: Vec::new(),
            nodes: Vec::new(),
            tables: Vec::new(),
        };
        let src = state.add_source(source);
        state.nodes.push(Node::default());
        (state, src)
    }

    /// Add a `Source` to the `State`.
    pub fn add_source(&mut self, source: Source) -> SourceId {
        let src = SourceId(self.sources.len());
        self.sources.push(source);
        src
    }

    /// Add a new leaf `Node` to the `State`.
    pub fn add_node(&mut self, parent: NodeId) -> NodeId {
        let node = NodeId(self.nodes.len());
        self.nodes.push(Node::default());
        self[parent].children.push(node);
        node
    }

    /// Add a new empty `Table` to the `State`.
    pub fn add_table<T: 'static>(&mut self) -> TableId<T> {
        let tbl = TableId(self.tables.len(), std::marker::PhantomData);
        self.tables.push(Box::new(Table::<T>(HashMap::new())));
        tbl
    }
}

/// A `Source` represents a unit of source code (for example, a `.iz` file).
pub struct Source {
    /// The location to use in error messages.
    pub name: String,
    /// The actual code, as an in-memory UTF-8 string.
    pub text: String,
}

/// Create a `Source` from a Rust `String` for testing.
#[macro_export]
macro_rules! text {
    ($text:expr) => {
        Source {
            name: ::std::string::String::from("text!"),
            text: ::std::string::String::from($text),
        }
    };
}

impl Source {
    /// Create a `Source` by reading a file.
    pub fn from_file(name: String) -> Result<Source> {
        match std::fs::read_to_string(&name) {
            Ok(text) => Ok(Source { name, text }),
            Err(_) => Err(format!("could not read {name}")),
        }
    }
}

/// A handle to one `Source` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SourceId(usize);

impl Index<SourceId> for State {
    type Output = Source;
    fn index(&self, SourceId(src): SourceId) -> &Source {
        &self.sources[src]
    }
}

/// A `Node` represents one element of the program that is being compiled.
#[derive(Default)]
pub struct Node {
    children: Vec<NodeId>,
}

/// A handle to one `Node` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

impl Index<NodeId> for State {
    type Output = Node;
    fn index(&self, NodeId(node): NodeId) -> &Node {
        &self.nodes[node]
    }
}

impl IndexMut<NodeId> for State {
    fn index_mut(&mut self, NodeId(node): NodeId) -> &mut Node {
        &mut self.nodes[node]
    }
}

/// A `Table` is used to store one type of information for `Node`s.
pub struct Table<T>(HashMap<NodeId, T>);

impl<T> Table<T> {
    /// Add information for a `NodeId`. Returns the old value if it exists.
    pub fn insert(&mut self, key: NodeId, val: T) -> Option<T> {
        self.0.insert(key, val)
    }

    /// Get information about a `NodeId`, if it exists.
    #[must_use]
    pub fn get(&self, key: NodeId) -> Option<&T> {
        self.0.get(&key)
    }
}

/// A handle to one `Table<T>` in the `State`.
#[derive(Debug, PartialEq)]
pub struct TableId<T>(usize, std::marker::PhantomData<T>);

// have to `impl` these separately because the `derive` depends on `T`
impl<T> Clone for TableId<T> {
    fn clone(&self) -> TableId<T> {
        *self
    }
}
impl<T> Copy for TableId<T> {}

impl<T: 'static> Index<TableId<T>> for State {
    type Output = Table<T>;
    fn index(&self, TableId(tbl, _): TableId<T>) -> &Table<T> {
        self.tables[tbl].downcast_ref().unwrap()
    }
}

impl<T: 'static> IndexMut<TableId<T>> for State {
    fn index_mut(&mut self, TableId(tbl, _): TableId<T>) -> &mut Table<T> {
        self.tables[tbl].downcast_mut().unwrap()
    }
}

/// A custom `Iterator` trait that can modify `State` and returns a `Result`.
pub trait Pass<T> {
    /// Process the next element.
    fn next(&mut self, state: &mut State) -> Result<Option<T>>;

    /// Run `next` until it returns `None`, ignoring the returned values.
    fn run(&mut self, state: &mut State) -> Result<()> {
        while self.next(state)?.is_some() {}
        Ok(())
    }
}

impl State {
    /// Get an iterator over `NodeId`s in the tree starting at `root`.
    #[must_use]
    pub fn postorder(&self, root: NodeId) -> Postorder {
        let mut out = Postorder(Vec::new());
        out.0.push((root, 0));
        out.dive(self);
        out
    }
}

/// An iterator over all `Node`s in a tree.
pub struct Postorder(Vec<(NodeId, usize)>);

impl Postorder {
    fn dive(&mut self, state: &State) {
        loop {
            let Some(&(node, idx)) = self.0.last() else {
                break;
            };
            let Some(&child) = state[node].children.get(idx) else {
                break;
            };
            self.0.push((child, 0));
        }
    }
}

impl Pass<NodeId> for Postorder {
    fn next(&mut self, state: &mut State) -> Result<Option<NodeId>> {
        let Some(&(node, _idx)) = self.0.last() else {
            return Ok(None);
        };
        self.0.pop();
        if let Some((_parent, idx)) = self.0.last_mut() {
            *idx += 1;
        }
        self.dive(state);
        Ok(Some(node))
    }
}
