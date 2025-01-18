# monkeylang-rs
Monkeylang implemented in Rust.

Monkey is a programming language that you can build yourself by reading through Writing An Interpreter In Go and Writing A Compiler In Go.
- <https://monkeylang.org/>
- <https://interpreterbook.com/>
- <https://interpreterbook.com/lost/>
- <https://compilerbook.com/>

Customized with the following:
- macro (from The Lost Chapter)
- quote/unquote as tokens
- loop (and break)
- comments with "//"
- escape characters with \\ (n, t, \\, ")
- puts no longer add newline
- exit (with exit code)
- additional builtins:
  - insert:
    - array: insert object at given index, shifting all elements after
    - hash : insert object at given key
  - replace: replace object in array at the given index
  - string: convert other objects to string object
  - map: for each object in array, run given function
- no magic semicolons
- deduplicate constants

