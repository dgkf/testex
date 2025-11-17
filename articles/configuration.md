# Configuration

To make the most of `testex`, there are a few configuration steps you
might consider. These are made simple by using:

``` r
testex::use_testex()
```

which will :

Add `packages = "testex"` to the `Roxygen` field in `DESCRIPTION`,
allowing `roxygen2` to make use of the `testex` tags.

Add `testex` as a `Suggests` dependency

Add settings to the `Config/testex/options` field in `DESCRIPTION`,
enabling example tests during `R CMD check` by default.

Add a `test-testex.R` test file if you’re using `testthat`, enabling
example tests during `testthat` test evaluation.

Though if you prefer you can configure all of this yourself:

## `testthat`

Running tests using `testthat` is simple. Just use

``` r
testex::use_testex_as_testthat()
```

This will add a simple, one-line file to your `tests/testthat` directory
containing

``` r
testex::test_examples_as_testthat()
```

By adding this single line to a `testthat` test file (such as
`tests/testthat/test-testex.R`), your example tests will be included as
part of your test suite.

When run this way, `testex` tests are embedded with additional metadata
including the original file location of the examples so that `testthat`
is able to provide more informative error messages.

## `R CMD check`

By default, your tests will run when your run examples using
`R CMD check`. However, `R CMD check` will stop on the first error and
truncates error output, which can be inconvenient for debugging. If
you’d prefer not to run tests during checking, you can add the following
line to your `DESCRIPTION`.

    Config/testex/options: list(check = FALSE)
