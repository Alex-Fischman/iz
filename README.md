# iz
WIP programming language

# spec
## Input
### Command Line
Takes the name of the file to use. Must be valid UTF-8.
#### Errors
No argument passed, file read failure.
### Output
An array of characters.

## Tokenizer
### Strings
Characters enclosed in " are treated as data instead of code.
#### Escapes
Inside strings, characters preceded by \ have special meanings.
- \\\\: backslash
- \\": double quote
- \n: newline
- \t: tab
#### Errors
Missing end quote, unknown escaped character.
### Comments
### Brackets
### Numbers
### Identifiers
### Output

## Parser
### Brackets
### Operators
### Output

## Analyzer
### Runtime Types
### Output

## Compiler
### Opcodes
### Output
