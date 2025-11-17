# Raise `testthat` Expectations With A Known Source Reference

Retroactively assigns a source file and location to a expectation. This
allows `testthat` to report an origin for any code that raised an
example test failure from the source `roxygen2` code, even though the
test code is reconstructed from package documentation files.

## Usage

``` r
with_srcref(src, expr, envir = parent.frame())
```

## Arguments

- src:

  A `srcref_key` which is parsed to produce an artificial
  [`srcref`](https://rdrr.io/r/base/srcfile.html) for the expectation
  signaled messages.

- expr:

  An expression to be evaluated. If an `expectation` condition is raised
  during its evaluation, its
  [`srcref`](https://rdrr.io/r/base/srcfile.html) is converted to `src`.

- envir:

  An environment in which to evaluate `expr`.

## Value

The result of evaluating `expr`, or an expectation with appended
[`srcref`](https://rdrr.io/r/base/srcfile.html) information if an
expectation is raised.
