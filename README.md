# CKaleidoscope

It's required that the type system be reconstructed
in order to fit the need of adding pointer types and
array types.

If you have some good ideas on that, please don't hesitate 
to [create an issue](https://github.com/seven-mile/CKaleidoscope/issues/new).

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
- [x] Pointers (such as &,* operator)
- [x] Full number types
- [x] Array type -> Compound array & pointer (even reference) type parsing
- [x] Implement a segment tree
- [x] Comma operator
- [x] Source Location
- [ ] `sizeof`,`alignof` operator
- [ ] Ternary operator
- [ ] Function (pointer) type
- [ ] DWARF support ☆ Source-level debug
- [x] Compile into executable file
- [ ] Label and `goto` [low prior]
- [ ] Structure / class

---

- [ ] Maybe: a VSCode extension for the toy language... (highlighting, language-server)

## Known Issues

- [ ] **segfault when data is too large (nothing to do with JIT)**
- [ ] for llvm-12, JIT will crash when looking up symbol `main`
- [ ] (Dev) CMake Debug lost Path information, which leads to linking error

## Examples

Please check the `test` directory.
