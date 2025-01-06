# huskeme 🐺
### A simple scheme interpreter including good error messages and an interactive Repl.<br>
I tried to base the implemented features as close as possible to the [r7rs language standard](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-1.html#TAG:__tex2page_toc).

## Usage
Evaluate a file with `huskeme <file>` and launch the Repl with `huskeme`

## Lang-features
- Data types: identifiers, lists (also dotted), functions, integers, strings, booleans
- Assigning and mutating variables
- Conditionals
- Closures using a variable number of arguments
- I/O, including loading files

## Features
#### Nice error messages using diagnose
<img width="448" alt="errores" src="https://github.com/user-attachments/assets/ef37f8ba-8213-4f36-a931-8d22c966503b" />

#### Repl with tab-completion using haskeline
`:help` lists all available commands in the Repl, tab-completion lists all available symbols in the environment

#### Basic standard library
The [stdlib](fixtures/lib.scm) can be loaded using `(load "<path>/lib.scm")` to use higher level concepts like mapping/folding etc. This list is not extensive.

## Tests
The [test-suite](test/Spec.hs) combines unit-tests with snapshot-tests from [fixtures/](fixtures)

## Todo
- [ ] Tail-call optimization
- [ ] LLVM Jit backend
- [ ] Macros
