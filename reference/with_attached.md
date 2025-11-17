# Temporarily attach a namespace

This function is primarily for managing attaching of namespaces needed
for testing internally. It is exported only because it is needed in code
generated within `Rd` files, but is otherwise unlikely to be needed.

## Usage

``` r
with_attached(ns, expr)
```

## Arguments

- ns:

  A namespace or namespace name to attach

- expr:

  An expression to evaluate while the namespace is attached

## Value

The result of evaluating `expr`
