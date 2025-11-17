# `testex` ***test examples*** ![](https://user-images.githubusercontent.com/18220321/211960830-8c969bee-8c80-4984-8132-b5993911f179.png)

[![CRAN](https://img.shields.io/cran/v/testex.svg)](https://cran.r-project.org/package=testex)
[![R CMD
check](https://github.com/dgkf/testex/workflows/R-CMD-check/badge.svg)](https://github.com/dgkf/testex/actions?query=workflow%3AR-CMD-check)
[![coverage](https://img.shields.io/codecov/c/github/dgkf/testex/main.svg)](https://app.codecov.io/gh/dgkf/testex)

Add tests and assertions in-line in examples

## Quick Start

Set up your package to use `testex` using

``` r
testex::use_testex()
```

and then start adding tests!

``` r
#' Hello, World!
#' 
#' @examples
#' hello("World")
#' @test "Hello, World!"
#'
#' hello("darkness my old friend")
#' @test grepl("darkness", .)
#' 
#' @export
hello <- function(who) {
  paste0("Hello, ", who, "!")
}
```

If you were already using `testthat`, you’ll immediately see a new test
context for testing your examples. And if you aren’t using `testthat`,
then you’ll find that your tests are being run with your examples when
you run `R CMD check`

## `roxygen2` tags

### `@test`

will check that the result of the last example is identical to your
test. You can use the example output in a function using a `.`.

``` r
#' @examples
#' sum(1:10)
#' @test 55
#' @test is.numeric(.)
```

### `@testthat`

is similar, but has the added benefit of automatically inserting a `.`
into `testthat::expect_*` functions.

``` r
#' @examples
#' sum(1:10)
#' @testthat expect_equal(55)
#' @testthat expect_vector(numeric())
```

## Prior Art

- [`roxytest`](https://github.com/mikldk/roxytest) A slightly different
  approach. Allows tests to be written in-line, but generates test files
  used directly by a testing framework.
