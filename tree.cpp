#ifndef TREE
#define TREE

#include <vector>

template <class V>
struct Tree {
	V value;
	std::vector<Tree<V>> children;
};

template <class V>
Tree<V> preorder(Tree<V>(*f)(const Tree<V>&), const Tree<V>& in) {
	Tree<V> out = f(in);
	for (uint i = 0; i < out.children.size(); ++i) 
		out.children[i] = postorder(f, out.children[i]);
	return out;
}

template <class V>
Tree<V> postorder(Tree<V>(*f)(const Tree<V>&), const Tree<V>& in) {
	Tree<V> out = in;
	for (uint i = 0; i < out.children.size(); ++i) 
		out.children[i] = postorder(f, out.children[i]);
	return f(out);
}

template <class T>
std::ostream& printTree(std::ostream& os, const Tree<T>& tree, int depth) {
	os << std::string(depth, '-') << '|' << tree.value << '\n';
	for (uint i = 0; i < tree.children.size(); i++) 
		printTree(os, tree.children[i], depth + 1);
	return os;
}

template <class T>
std::ostream& std::operator<<(std::ostream& os, Tree<T>& tree) {
	return printTree(os, tree, 0);
}

#endif
