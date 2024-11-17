# iz: a programming language for me

## Goals
- Adaptable: I don't know what I'll want in the future. A language for me needs to support both type theory and custom allocators.
- Performant: iteration time and runtime are both important to me.
- Small: I prefer systems with minimal feature sets that can be extended.

## Compiler
- There’s a consistent intermediate representation across all passes, which is a Tree
- A Tree has a slice of source text for errors, a list of children Trees, and arbitrary data
- The initial Tree has a 0..0 slice, one child leaf for each character, and no data
- The final Tree has a 0..0 slice, no children, and executable data
- Each compiler pass is a Tree mutator, which may return errors
- Each compiler pass should recur depth-first postorder

## Standard Library
- If statements, while loops, etc.
- Operator overloading and definition
- Find-and-replace hygienic macros
- Namespaces
	- `use` imports any scope
	- All structs have a scope that contains their fields and methods
	- Struct scopes can be used for faking inheritance with composition

## Todo
Formalize the idea of contexts and scopes and passes
