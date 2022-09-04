WIP programming language

# Input
## Command Line
Takes the name of the file to use. Must be valid UTF-8.
#### Errors
No argument passed, file read failure.
## Output
A list of characters.

# Tokenizer
## Strings
Characters enclosed in " are treated as data instead of code.
#### Escapes
Inside strings, characters preceded by \ have special meanings.
- \\\\: backslash
- \\": double quote
- \n: newline
- \t: tab
#### Errors
Missing end quote, unknown escaped character.
## Comments
Characters after a # but before newlines (or the end of the file) are ignored.
## Brackets
(, ), {, }, [, and ] are always their own tokens.
## Whitespace
Tabs, spaces, and newlines are ignored.
## Groups
All other characters are grouped into tokens consecutively.
If the group is composed of only 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, and _, with an optional preceding -, then it is an integer constant.
Otherwise, it is an identifier.
## Output
A list of tokens, where each token has:
- a location: row, column, length, and file name reference for errors
- an enum that holds different data based on the type of the token:
    - identifier: no additional information necessary
    - string: no additional information necessary
        - not comparable with identifiers; must account for escapes
    - open_bracket, close_bracket: an enum for round, curly, or square brackets
    - number: the integer value of the number that appeared in code

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
