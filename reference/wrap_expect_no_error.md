# Wraps an example expression in a `testthat` expectation to not error

Wraps an example expression in a `testthat` expectation to not error

## Usage

``` r
wrap_expect_no_error(expr, value)
```

## Arguments

- expr:

  An expression to wrap in a `expect_no_error()` expectation. Uses
  `testthat`s version if recent enough version is available, or provides
  a fallback otherwise.

- value:

  A symbol to use to store the result of `expr`

## Value

A
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
call
