# `vapply` shorthand alternatives

Simple wrappers around `vapply` for common data types

## Usage

``` r
vlapply(..., FUN.VALUE = logical(1L))

vcapply(..., FUN.VALUE = character(1L))

vnapply(..., FUN.VALUE = numeric(1L))
```

## Arguments

- ...:

  Arguments passed to [`vapply`](https://rdrr.io/r/base/lapply.html)

- FUN.VALUE:

  A preset signature for the flavor of
  [`vapply`](https://rdrr.io/r/base/lapply.html). This is exposed for
  transparency, but modifying it would break the implicit contract in
  the function name about the return type.

## Value

The result of [`vapply`](https://rdrr.io/r/base/lapply.html)
