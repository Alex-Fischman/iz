WIP Programming Language

# Goals
- Simplicity before complexity
- Semantics: stack based
- Syntax: brackets and operators

# Tokens
- Comments: # to \n
- Whitespace: spaces, tabs, newlines
- Brackets: (, ), {, }, \[, \]
- Strings: "text with whitespace and \" escapes"
- Integers: -1_000_000
- Identifiers: asdf123_asdf, +, if

# Parser
- Brackets: move tokens between brackets into children
- Operators: move tokens around certain identifiers into children

# Rewrite
- Operators: move children to precede operators, replace names with funcs

# Analysis
- Scope
- Types

# Bytecode
- Intrinsics: operations on the stack
- Labels: surround code blocks

# Backend
- x86_64
- RISC-V
