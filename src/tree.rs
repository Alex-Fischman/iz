//! The intermediate representation

/// Represents the source code of an iz program, e.g. a .iz file
pub struct Source {
    /// The name of the source, e.g. a file name
    pub name: String,
    /// The text of the source, e.g. the file's contents
    pub text: String,
}

/// Represents one part of the source code of an iz program
#[derive(Clone, Copy)]
pub struct Slice<'a> {
    /// A reference to the `Source`
    pub source: &'a Source,
    /// The byte index of the start of the slice
    pub lo: usize,
    /// The byte index of the end of the slice (exclusive)
    pub hi: usize,
}

impl<'a> Slice<'a> {
    /// Create a new slice
    pub fn new(source: &Source, lo: usize, hi: usize) -> Slice {
        Slice { source, lo, hi }
    }

    /// Get the string that this slice represents
    pub fn str(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl<'a> std::fmt::Display for Slice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        write!(f, "{:?} at {}:{row}:{col}", self.str(), self.source.name)
    }
}

/// An identifier for a node of a `Tree`
#[derive(Clone, Copy)]
pub struct Node(usize);

struct Annotation<T>(Vec<T>);

impl<T> Annotation<T> {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn push(&mut self, x: T) {
        self.0.push(x);
    }
}

impl<T> std::ops::Index<Node> for Annotation<T> {
    type Output = T;
    fn index(&self, Node(i): Node) -> &T {
        &self.0[i]
    }
}

impl<T> std::ops::IndexMut<Node> for Annotation<T> {
    fn index_mut(&mut self, Node(i): Node) -> &mut T {
        &mut self.0[i]
    }
}

/// The intermediate representation of a program
pub struct Tree<'a> {
    slices: Annotation<Slice<'a>>,
    children: Annotation<Vec<Node>>,
}

/// The identifier for the root node of any `Tree`
pub const ROOT: Node = Node(0);

/// The type of a compiler pass
pub type Pass<'a> = Box<dyn Fn(&mut Tree, Node) -> Result + 'a>;
/// An specialized alias for the return value of compiler passes
pub type Result = std::result::Result<(), String>;

impl<'a> Tree<'a> {
    /// Create a new, empty `Tree`
    pub fn new_tree(slice: Slice) -> Tree {
        Tree {
            slices: Annotation(vec![slice]),
            children: Annotation(vec![vec![]]),
        }
    }

    /// Create a new child node
    pub fn new_child(&mut self, parent: Node, slice: Slice<'a>) -> Node {
        let child = Node(self.slices.len());
        self.slices.push(slice);
        self.children.push(vec![]);
        self.children[parent].push(child);
        child
    }

    /// Get the children of a node
    pub fn get_children(&self, parent: Node) -> &[Node] {
        &self.children[parent]
    }

    /// Get a mutable vector of the children of a node
    pub fn get_children_mut(&mut self, parent: Node) -> &mut Vec<Node> {
        &mut self.children[parent]
    }

    /// Get the `Slice` of a `Node`
    pub fn get_slice(&self, node: Node) -> Slice<'a> {
        self.slices[node]
    }

    /// Set the `Slice` of a `Node`
    pub fn set_slice(&mut self, node: Node, slice: Slice<'a>) {
        self.slices[node] = slice;
    }

    /// Run a `Pass` over all the children of a `Node`
    pub fn run_pass_over_children(&mut self, parent: Node, pass: &Pass) -> Result {
        let children: Vec<_> = self.get_children(parent).to_vec();
        for &child in &children {
            pass(self, child)?;
        }
        Ok(())
    }
}
