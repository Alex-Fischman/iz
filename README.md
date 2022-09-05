WIP programming language

# Parser
## Brackets
Tokens between (), {}, or [] are separated from the code on the outside.
#### Errors
Missing end bracket, wrong bracket type.
## Operators
Some identifiers are treated as operators, where their arguments are moved into their child lists and the symbol is evaluated as a different identifier.
#### Errors
Missing arguments.
## Output
A tree of tokens, where each token has:
- a location: row, column, length, and file name reference for errors
- an enum that holds different data based on the type of the token:
    - identifier: same as tokenizer
    - string: same as tokenizer
    - number: same as tokenizer
    - brackets: bracket type, list of children
    - operator: new identifier, list of children


# Analyzer
## Runtime Types
## Output

# Compiler
## Opcodes
## Output
