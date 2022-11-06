WIP Programming Language

# Goals
- Simplicity before complexity
- Semantics: stack based
- Syntax: brackets and operators

# Tokens
- Brackets: (, ), {, }, \[, \]
- Strings: "text with whitespace and \" escapes"
- Integers: -1_000_000
- Identifiers: asdf123_asdf, +, if

# Parser
- Brackets: move tokens between brackets into children
- Operators: move tokens around certain identifiers into children

# Rewrite
- Operators: move children to follow operators, including parens
	- only {, }, \[, and \] should have children

# Analysis
- Scope
- Types
