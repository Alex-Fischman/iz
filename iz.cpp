#include <fstream>
#include <iostream>
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

struct Token {
	enum { TODO, Ignored, String, Operator, Identifier } type;
	std::string str; int pos;
};

std::ostream& operator<<(std::ostream& os, const Token& t) {
	return os << t.str;
}

std::vector<Token> tagIgnoredChars(const std::vector<Token>& in) {
	std::vector<Token> out;
	for (Token t : in) {
		if (t.str == " " || t.str == "\n" || t.str == "\t" || 
		    t.str == "\r" || t.str == "," || t.str == ";") 
		     out.push_back({Token::Ignored, t.str});
		else out.push_back(t);
	}
	return out;
}

std::vector<Token> tagComments(const std::vector<Token>& in) {
	std::vector<Token> out;
	for (uint i = 0; i < in.size(); i++) {
		if (in[i].str == "~") {
			std::string end = in[++i].str == "~"? "\n": "~";
			while (in[i].str != end) i++;
			out.push_back({Token::Ignored});
		}
		else out.push_back(in[i]);
	}
	return out;
}

struct Operator {
	std::string name; std::string func;
	int prec; bool left; int arity; int pos;
};

struct Brackets {
	std::string open; std::string close;
	std::string func;
};

std::vector<Operator> operators;
std::vector<Brackets> brackets;

std::vector<Token> tagDirectives(const std::vector<Token>& in) {
	std::vector<Token> out;
	for (uint i = 0; i < in.size(); i++) {
		if (in[i].str == "#") {
			std::vector<std::string> args = {""};
			for (; in[i].str != "\n"; i++) {
				if (in[i].str == " ") args.push_back("");
				else args.back().append(in[i].str);
			}
			if (args[0] == "#operator") operators.push_back({args[1], args[2], 
				std::stoi(args[3]), args[4] == "l", 
				std::stoi(args[5]), std::stoi(args[6])
			});
			else if (args[0] == "#brackets") brackets.push_back({
				args[1], args[2], args[3]
			});
			out.push_back({Token::Ignored});
		}
		else out.push_back(in[i]);
	}
	return out;
}

std::vector<Token> groupStrings(const std::vector<Token>& in) {
	std::vector<Token> out;
	for (uint i = 0; i < in.size(); i++) {
		if (in[i].str == "\"") {
			Token token = {Token::String, "", in[i].pos};
			while (in[++i].str != "\"") {
				if (in[i].str == "\\") {
					if (in[++i].str == "\\")    token.str += '\\';
					else if (in[i].str == "n")  token.str += '\n';
					else if (in[i].str == "r")  token.str += '\r';
					else if (in[i].str == "t")  token.str += '\t';
					else if (in[i].str == "\"") token.str += '"';
				}
				else token.str += in[i].str;
			}
			out.push_back(token);
		}
		else out.push_back(in[i]);
	}
	return out;
}

std::vector<Token> groupOperators(const std::vector<Token>& in) {
	std::vector<std::string> names;
	for (Operator o : operators) {
		names.push_back(o.name);
	}
	for (Brackets b : brackets) {
		names.push_back(b.open);
		names.push_back(b.close);
	}
	
	std::sort(names.begin(), names.end(), [] (auto a, auto b) {
		return a.size() > b.size();
	});

	std::vector<Token> out = in;
	for (uint i = 0; i < out.size(); i++) {
		for (std::string n : names) {
			std::string slice = "";
			for (uint k = 0; k < n.size(); k++) slice += out[i + k].str;
			if (n == slice) {
				out[i].type = Token::Operator;
				out[i].str = n;
				out.erase(out.begin() + i + 1, out.begin() + i + n.size());
				break; // CLEAN
			}
		}
	}

	return out;
}

std::vector<Token> groupIdentifiers(const std::vector<Token>& in) {
	std::vector<Token> out;
	Token acc;
	for (Token t : in) {
		if (t.type == Token::TODO) {
			if (acc.str == "") acc = {Token::Identifier, t.str, t.pos};
			else acc.str += t.str;
		}
		else {
			if (acc.str != "") {
				out.push_back(acc);
				acc.str = "";
			}
			out.push_back(t);
		}
	}
	return out;
}

std::vector<Token> filterIgnored(const std::vector<Token>& in) {
	std::vector<Token> out;
	for (Token t : in) if (t.type != Token::Ignored) out.push_back(t);
	return out;
}

using AST = Tree<Token>;

// CLEAN
AST parseBrackets(const AST& in) {
	AST out = {in.value};
	std::vector<std::pair<std::string, AST*>> stack = {{"", &out}};
	for (uint i = 0; i < in.children.size(); i++) {
		std::string ending = "";
		for (uint j = 0; j < brackets.size(); j++) 
			if (in.children[i].value.str == brackets[j].open) 
				ending = brackets[j].close;

		if (ending != "") {
			stack.back().second->children.push_back({in.children[i]});
			stack.push_back({ending, &stack.back().second->children.back()});
		}
		else if (in.children[i].value.str == stack.back().first) stack.pop_back();
		else stack.back().second->children.push_back({in.children[i]});
	}
	return out;
}

AST parseOperators(const AST& in) {
	std::vector<std::pair<int, Operator>> opsInAST;
	for (uint i = 0; i < in.children.size(); i++) 
		for (uint j = 0; j < operators.size(); j++) 
			if (in.children[i].value.str == operators[j].name) 
				opsInAST.push_back({i, operators[j]});

	std::stable_sort(opsInAST.begin(), opsInAST.end(), [] (auto a, auto b) {
		return a.second.prec > b.second.prec || 
			(a.second.prec == b.second.prec && a.second.left);
	});

	AST out = in;
	for (uint i = 0; i < opsInAST.size(); i++) {
		int start = opsInAST[i].first - opsInAST[i].second.pos;
		int end   = start + opsInAST[i].second.arity;
		for (int j = start; j < end; j++) if (j != opsInAST[i].first) {
			out.children[opsInAST[i].first].children.push_back(out.children[j]);
			out.children.erase(out.children.begin() + j);
			for (uint k = 0; k < opsInAST.size(); k++) 
				if (opsInAST[k].first > j) --opsInAST[k].first;
		}
	}
	return out;
}

AST parseCalls(const AST& in) {
	if (in.value.str == "root") return in;
	if (in.value.str == "$") return {{Token::Identifier, "call"}, in.children};
	
	std::string func = "";
	for (Operator o : operators) if (o.name == in.value.str) func = o.func;
	for (Brackets b : brackets)  if (b.open == in.value.str) func = b.func;
	if (func == "") return in;

	AST out = {{Token::Identifier, "call"}};
	out.children.push_back({{Token::Identifier, func}});
	for (AST c : in.children) out.children.push_back(c);
	return out;
}

int main(int argc, char const *argv[]) {
	if (argc != 3) return 1;

	// Input
	std::vector<Token> tokens;
	std::ifstream ifs (argv[1], ifs.in);
	char c;
	for (int i = 0; ifs.get(c); i++) tokens.push_back({Token::TODO, {c}, i});
	ifs.close();

	// Tokenization
	tokens = tagIgnoredChars (tokens);
	tokens = tagComments     (tokens);
	tokens = tagDirectives   (tokens);
	tokens = groupStrings    (tokens);
	tokens = groupOperators  (tokens);
	tokens = groupIdentifiers(tokens);
	tokens = filterIgnored   (tokens);

	// Parsing
	AST ast = {{Token::Ignored, "root", -1}};
	for (Token t : tokens) ast.children.push_back({t});
	ast = parseBrackets(ast);
	ast = postorder(parseOperators, ast);
	ast = postorder(parseCalls, ast);

	std::cout << ast;

	return 0;
}

std::pair<int, int> filePos(std::istream& is, int p) {
	std::streampos cur = is.tellg();
	is.seekg(0);
	std::pair<int, int> out;
	while (is.tellg() != p) {
		if (is.get() == '\n') {
			out.first++;
			out.second = 1;
		}
		else out.second++;
	}
	is.seekg(cur);
	return out;
}
