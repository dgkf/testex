# Run examples as `testthat` expectations

Run examples as `testthat` expectations

## Usage

``` r
use_testex_as_testthat(path = getwd(), context = "testex", quiet = FALSE)
```

## Arguments

- path:

  A package source code working directory

- context:

  A `testthat` test context to use as the basis for a new test filename.

- quiet:

  Whether to emit output messages.

## Value

The result of [`writeLines()`](https://rdrr.io/r/base/writeLines.html)
after writing a new `testthat` file.
