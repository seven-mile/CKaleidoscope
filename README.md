# CKaleidoscope

It's required that the type system be reconstructed
in order to fit the need of adding pointer types and
array types.

If you have some good ideas on that, please don't hesitate 
to [create a issue](https://github.com/seven-mile/CKaleidoscope/issues/new).

[LLVM tutorial](http://llvm.org/docs/tutorial/)

[Advanced tutorial](https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/README.html)

## Todo

- [x] Lexer with any token & debug outputing
- [x] Basic AST definition & types
- [x] Basic parser
- [x] Type extension (only double for all numbers)
- [x] Variable & Assignment
- [x] Braced code block
- [x] String type
- [x] Constants: string, char
- [x] Stdlib support: std IO, file IO (directly using C-API)
- [x] If statement
- [x] For statement
- [ ] Switch statement
- [x] Return statement
- [ ] Pointers (such as &,* operator)
- [ ] Full number types
- [ ] Array type
- [ ] Function (pointer) type
- [ ] DWARF support ☆ Source-level debug
- [ ] Compile into executable file,

---

- [ ] Maybe: a VSCode extension for the toy language... (highlighting, language-server)

## Examples

Please check the `test` directory.
