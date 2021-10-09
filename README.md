# varlang

This program aims to explore variability-aware programming.

As such, we implement a simple variational programming language.

## Motivation

Suppose we have a function `twice` which doubles its parameter.
We can implement it in at least two ways.

```
twice(x) = x + x

twice(x) = 2 * x
```

One implementation may be preferable over the other, depending
on the circumstance.
A C programmer would express this implementation dependence using
pre-processor macros.

```c
#ifdef IMPLPLUS
int twice(x) {
  return x + x;
}
#endif

#ifdef IMPLMULT
int twice(x) {
  return 2 * x
}
#endif
```

This is the way variability is done in most real-world use cases.
The study of variability-aware software, however, aims to implement
variability in a more disciplined way, which allows us to better
understand the behavior of such variability.

In this language, whose approach to variability is based on the
[Choice Calculus](https://web.engr.oregonstate.edu/~walkiner/projects/choice-calculus.html), we could express the variable implementation as follows

```
twice(x) =
  dim Impl<plus, times> in
    Impl<x + x, 2 * x>
```

Which results in the following AST.
```
dim Impl<plus, times> in
  Decl "twice" ["x"]
    ( VExpr Impl<Add {e1 = Var "x", e2 = Var "x"}
    , Mult {e1 = Num 2, e2 = Var "x"}>
    )
```

