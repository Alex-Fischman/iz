# iz: a programming language (for me)

## Motivation

I want to create a language that I, personally, will enjoy using for the foreseeable future.

No one can predict what will be needed in a programming language in a decade. Therefore, if there’s one quality that iz must have, it is adaptability. I want to be able to use this language for any personal project that I can think of. Also, this needs to be a project I can finish. I don’t want to keep writing a programming language for the rest of my life, I want to write a language that I can be productive in.

For a truly adaptable language to be complete, the programmer must be able to define the language to almost the same degree that the compiler writer did. The compiler itself should be as minimal as possible and have most of its features come from a default prelude. This would give the user the most flexibility because the prelude could then be swapped for a custom one.

A minimal compiler will be an almost direct translation from text to bytecode. The base bytecode will model a very simple virtual machine that only has a program counter and stack pointer. I think that this is the closest that we can get to the hardware without sacrificing too much portablility.

## Implementation

### Definitions

- Let `Data` be a dynamically typed map.
- Let `Node` be a wrapped integer type.
- Let `Context` be a map from `Node`s to `Data`s.
	- A `Context` is interpreted as an adjacency list representation of a syntax tree, where each node can hold arbitrary data.
	- To do this, every `Data` contains a list of the children of the current node, and the 0 `Node` is treated as the root.
- Let a `Pass` be a function that modifies a `Context`.
	- A `Pass` is implemented in Rust as a `Fn(&mut Context)`, but it could just as easily be a `Fn(&Context) -> Context`.
	- There are many, many invariants that every `Pass` must hold, such as the list of `Pass`es to be run in the root, or each `Node` holding a list of its children.
- Let an `Interpreter` be a function that takes a `Context` and runs it as a `Pass`.
	- The `Context` must have the form where each child of the root contains a list of `Instruction`s, and has no children.
	- The `Interpreter` doesn't strictly have to be an interpreter, it could be a JIT compiler or something similar.
- Let the "frontend" refer to the list of `Pass`es.
- Let the "backend" refer to a function that takes a `Context` and performs a side effect with it.
	- The `Interpreter` can be a backend, albeit a weird one.

### Future

Here is a list of things that should be possible for users of this programming language to add:
- If statements, while loops, for loops
- Variables and namespacing
- Type checking
- Operator overloads for Vectors, Rationals, etc.
- Shader restrictions
- “Lifetime analysis” or other memory safety guarantees
- Proof system?

Here are some more thoughts and plans for the future

Macros:
- a macro is just a function that runs at compile time
- generics are macros that take types and return types
- Rust macros are macros that take ASTs and return ASTs
- all functions can be macros, but not all macros are functions
- compile passes are macros that take a mutuable Context struct
- compile passes can schedule future compile passes by modifying Context

Syntax:
- operators call macros when parsed, which usually just unroll the arguments but can do anything
- ()s are used for arbitrary grouping in parsing
- {}s are used to quote code blocks and turn them into functions/macros

Namespaces:
- `use` imports any scope
- all structs have a scope that contains their fields and methods
- can be used for faking inheritance with composition

Colorability:
- const, mut, async, comptime, etc. are all things that end up coloring functions
- the only solution that I know of is to choose a default and auto-convert from it, e.g.
	- all pointers are const pointers
	- no await syntax
	- all values can be used at compile time
