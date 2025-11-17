# A syntactic helper for writing quick and easy example tests

A wrapper around `stopifnot` that allows you to use `.` to refer to
`.Last.value` and preserve the last non-test output from an example.

## Usage

``` r
testex(
  ...,
  srcref = NULL,
  example_srcref = NULL,
  value = get_example_value(),
  envir = parent.frame(),
  style = "standalone"
)
```

## Arguments

- ...:

  Expressions to evaluated. `.` will be replaced with the expression
  passed to `val`, and may be used as a shorthand for the last example
  result.

- srcref:

  An option `srcref_key` string used to indicate where the relevant test
  code originated from.

- example_srcref:

  An option `srcref_key` string used to indicate where the relevant
  example code originated from.

- value:

  A value to test against. By default, this will use the example's
  `.Last.value`.

- envir:

  An environment in which tests should be evaluated. By default the
  parent environment where tests are evaluated.

- style:

  A syntactic style used by the test. Defaults to `"standalone"`, which
  expects `TRUE` and uses a `.`-notation. Accepts one of `"standalone"`
  or `"testthat"`. By default, styles will be implicitly converted to
  accommodate known testing frameworks, though this can be disabled by
  passing the style `"AsIs"` with
  [`I()`](https://rdrr.io/r/base/AsIs.html).

## Value

Invisibly returns the `.Last.value` as it existed prior to evaluating
the test.

## Documenting with `testex`

`testex` is a simple wrapper around execution that propagates the
`.Last.value` returned before running, allowing you to chain tests more
easily.

### Use in `Rd` files:

    \examples{
      f <- function(a, b) a + b
      f(3, 4)
      \testonly{
        testex::testex(
          is.numeric(.),
          identical(., 7)
        )
      }
    }

But `Rd` files are generally regarded as being a bit cumbersome to
author directly. Instead, `testex` provide helpers that generate this
style of documentation, which use this function internally.

### Use with `roxygen2`

Within a `roxygen2` `@examples` block you can instead use the `@test`
tag which will generate Rd code as shown above.

    #' @examples
    #' f <- function(a, b) a + b
    #' f(3, 4)
    #' @test is.numeric(.)
    #' @test identical(., 7)
