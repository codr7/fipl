# fipl

### intro
fipl is an interpreted language in the spirit of Forth, implemented and embedded in Common Lisp.

### setup
The REPL used for all examples in this document may be launched by evaluating `(fipl:repl)` after loading the project,
or by using one of the provided [binaries](https://github.com/codr7/fipl/tree/main/bin).

### build
Building an executable in the current directory goes something like this:

```
$ sbcl --eval "(asdf:operate :build-op 'fipl)"
```

### stack operations
Values are pushed on a thread local stack.

```
  1 2 3

[1 2 3]
  4 5 6

[1 2 3 4 5 6]
```

`d` may be used to drop the last value.

```
  1 2 3 d

[1 2]
```

`cp` may be used to copy the last value.

```
  1 2 3 cp

[1 2 3 3]
```

`swap"` may be used to swap the last two values.

```
  1 2 3 swap

[1 3 2]
```

### logics
`t` and `f` may be used to obtain true and false values, `t`is mapped to `T` on the Lisp side and `f` to `NIL`.
Every value has a boolean representation, for integers 0 is `f` and everything else `t`.

#### operators
The usual short circuiting logic operators are provided, the left hand side is taken from the stack but the right hand side needs to be parsed as the preceding source token to enable short circuiting.

```
  t 42 and

[42]
```

```
  f 42 or

[42]
```

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