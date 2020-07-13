# CKaleidoscope

A practice of compiler and language advanced features

nothing... just containing some interesting tricks?

[Powerful llvm tutorial](http://llvm.org/docs/tutorial/)


## Todo

- [x] Lexer with any token & debug outputing
- [x] Basic AST definition & types
- [x] Basic parser
- [x] Type extension
- [ ] Variable & Assignment
- [ ] `typeof` operator
- [ ] Braced code block
- [ ] Function type
- [x] String type
- [x] Constants: string, char
- [ ] Stdlib support: std IO, file IO
- [ ] Array type

## Grammar Intro

### types

only 4, `number`, `string`, `char`, `bool`.

`number` = double,

`char` = int_8

`string` = int_8[] (with null)

`bool` = int_1

### extern:

1. math

```
extern number sin(number x);
extern number log(number x);
extern number tgamma(number x);
```

or other c lib

2. STD_API

```
extern number print(string str);
print("hello, world");
```

wrote in `stdlib.cpp`, wrapped functions for io, file io, etc. .

### if:

```
def number sgn(number x): if x>0 then 1 else if x<0 then -1 else 0;
```

### for:

```
for i=1;i<7;1 in printd(i);
```

```
for i=6;i>0;-0.5 in printd(i);
```

### advanced example

```
extern number tgamma(number x);
extern number lgamma(number x);
extern number print(string str);
extern number printd(number val);
def number gamma(number x, bool log): if log then lgamma(x) else tgamma(x);
for i=1;i<6 in print("tgamma(") + printd(i) + print(") = ") + printd(gamma(i, false)) + print("\n");
for i=1;i<6 in print("lgamma(") + printd(i) + print(") = ") + printd(gamma(i, true)) + print("\n");

```

it gives

```
tgamma(1.000000) = 1.000000
tgamma(2.000000) = 1.000000
tgamma(3.000000) = 2.000000
tgamma(4.000000) = 6.000000
tgamma(5.000000) = 24.000000
tgamma(6.000000) = 120.000000

lgamma(1.000000) = 0.000000
lgamma(2.000000) = 0.000000
lgamma(3.000000) = 0.693147
lgamma(4.000000) = 1.791759
lgamma(5.000000) = 3.178054
lgamma(6.000000) = 4.787492

```
