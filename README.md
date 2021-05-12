# fipl

### intro
fipl is an interpreted language in the spirit of Forth, implemented and embedded in Common Lisp.

### setup
The REPL used for all examples in this document may be launched by evaluating `(fipl:repl)` after loading the project.

### booleans
`t` and `f` may be used to obtain true and false values, `t`is mapped to `T` on the Lisp side and `f` to `NIL`.

### functions
Functions are called on reference by default.

```
  42 cp
  
[42 42]
```

Identifiers may be prefixed with `&` to obtain obtain the actual function as a value.

```
  42 &cp call
  
[42 42]
```