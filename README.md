# iz: a programming language (for me)

I want to create a language that I, personally, will enjoy using for the foreseeable future.

No one can predict what will be needed in a programming language in a decade. Therefore, if there’s one quality that iz must have, it is adaptability. I want to be able to use this language for any personal project that I can think of. Also, this needs to be a project I can finish. I don’t want to keep writing a programming language for the rest of my life, I want to write a language that I can be productive in.

For a truly adaptable language to be complete, the programmer must be able to define the language to almost the same degree that the compiler writer did. Relatedly, since it seems impossible to me for the compiler to always work if the programmer can remove phases, it follows that the compiler itself must be as minimal as possible and have most of its features come from a default prelude. This would give the user the most flexibility because the prelude could then be swapped for a custom one.

A minimal compiler will be an almost direct translation from text to bytecode. The base bytecode will model a machine that has a program counter and stack pointer but no other registers. I believe that this is the closest that we can get to the hardware without sacrificing too much portablility.

Should there be a difference between macros and compiler passes?

Here is a list of things that should be possible for users of this programming language to add:
- Auto-polyfilled intrinsics: a minimal backend will only need to support a tiny set of base ops, but faster backends can choose to translate other types of ops
- If statements, while loops, for loops
- Variables and namespacing
- Type checking: do you get free dependent types?
- Operator overloads for Vectors, Rationals, etc.
- Shader restrictions
- “Lifetime analysis” or other memory safety guarantees
- Proof system?

Here are some more thoughts and plans for the future

Macros:
- a macro is just a function that runs at compile time
- generics are macros that take types and return types
- Rust macros are macros that take ASTs and return ASTs
- all runtime functions are compile time functions
- but some compile time functions will take compile-time-only args
- compile passes are macros that take an AST and a Data object

Syntax:
- arbitrary operator definitions
- operators call macros when parsed, which usually just unroll the arguments but can do anything
- ()s are used for arbitrary grouping in parsing
- {}s are used to quote code blocks and turn them into functions/macros

Namespaces:
- `use` imports any scope
- all structs have a scope that contains their fields and methods
- can be used for faking inheritance with composition

Colorability:
- const, mut, async, comptime, etc. are all things that end up coloring functions
- the only solution that I know of is to choose a default and auto-convert from it
- for example, all pointers are const pointers, all async functions can be called synchronously (by blocking), all values can be used at compile time, etc.
