# Expect no Error

Expect no Error

## Usage

``` r
fallback_expect_no_error(object, ...)
```

## Arguments

- object:

  An expression to evaluate

- ...:

  Additional arguments unused

## Value

The value produced by the expectation code

## Note

This is a stop-gap implementation, and will only be used for legacy
versions of `testthat` before this was properly supported.

A `testthat` expectation that the provided code can be evaluated without
producing an error. This is the most basic expectation one should expect
of any example code. Further expectations are provided in subsequent
`testthat` code.
