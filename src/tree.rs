//! The intermediate representation

/// Represents the source code of an iz program, e.g. a .iz file
pub struct Source {
    /// The name of the source, e.g. a file name
    pub name: String,
    /// The text of the source, e.g. the file's contents
    pub text: String,
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
    source: &'a Source,
    ranges: Annotation<(usize, usize)>,
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
    pub fn new_tree(source: &Source) -> Tree {
        Tree {
            source,
            ranges: Annotation(vec![(0, 0)]),
            children: Annotation(vec![vec![]]),
        }
    }

    /// Create a new child node
    pub fn new_child(&mut self, parent: Node, lo: usize, hi: usize) -> Node {
        let child = Node(self.ranges.len());
        self.ranges.push((lo, hi));
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

    /// Get the range of a `Node`
    pub fn get_range(&self, node: Node) -> (usize, usize) {
        self.ranges[node]
    }

    /// Set the range of a `Node`
    pub fn set_range(&mut self, node: Node, range: (usize, usize)) {
        self.ranges[node] = range;
    }

    /// Get the text of the source code that labels a node
    pub fn get_slice(&self, node: Node) -> &str {
        &self.source.text[self.ranges[node].0..self.ranges[node].1]
    }

    /// Get a printable representation of a node, containing location information
    pub fn get_printable(&self, node: Node) -> String {
        let range = self.ranges[node];
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == range.0 => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        format!(
            "{:?} at {}:{row}:{col}",
            self.get_slice(node),
            self.source.name
        )
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
