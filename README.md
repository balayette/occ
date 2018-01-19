# occ
occ stands for OCaml C Compiler.

# What
A C nanopass compiler written in OCaml (for fun).
It uses [Sedlex](https://github.com/alainfrisch/sedlex), [Menhir](http://gallium.inria.fr/~fpottier/menhir/), and [Nanocaml](https://github.com/nanocaml/nanocaml)

# Features :

- [ ] Parsing :
  - [x] Function declaration
    - [x] With parameters
  - [x] Variable declaration
    - [x] Array declaration
  - [ ] Variable assignement
  - [ ] Loops
    - [x] While
    - [ ] For
    - [ ] Do while
  - [ ] Conditionals
    - [x] if, else, else if
    - [ ] Ternary
  - [x] Var access
    - [x] Simple access
    - [x] Array access
    - [x] Dereference
  - [x] Function calls
    - [x] Function calls with parameters
  - [x] Arithmetic
    - [x] Basic operations
    - [x] Operator precedance
  - [ ] Operators
    - [ ] &&
    - [ ] ||
    - [x] <=, >=, <, >, ==
    - [ ] !=
    - [ ] !
    - [ ] ~
    - [ ] ^
    - [ ] &
    - [ ] |
    - [ ] <<
    - [ ] >>
- [ ] All the unlisted features

- [ ] Compilation
  - [x] Function declaration
    - [ ] With parameters
  - [ ] Variable declaration
    - [ ] Array declaration
  - [ ] Variable assignement
  - [ ] Loops
    - [ ] While
    - [ ] For
    - [ ] Do while
  - [ ] Conditionals
    - [x] if, else, else if
    - [ ] Ternary
  - [ ] Var access
    - [ ] Simple access
    - [ ] Array access
    - [ ] Dereference
  - [x] Function calls
    - [ ] Function calls with parameters
  - [x] Arithmetic
    - [x] Basic operations
    - [x] Operator precedance
  - [ ] Operators
    - [ ] &&
    - [ ] ||
    - [ ] <=, >=, <, >, ==
    - [ ] !=
    - [ ] !
    - [ ] ~
    - [ ] ^
    - [ ] &
    - [ ] |
    - [ ] <<
    - [ ] >>
- [ ] All the unlisted features

# Disclaimer
- I know nothing about compiler architecture.
- I know very little about assembly.
- The code is garbage held together by duct tape and the OCaml typesystem.