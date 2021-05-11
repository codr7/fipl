# fipl

### intro
fipl is an interpreted language in the spirit of Forth, deeply embedded in Common Lisp.

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