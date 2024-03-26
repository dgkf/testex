# `testex` ***test examples*** <img src="https://user-images.githubusercontent.com/18220321/211960830-8c969bee-8c80-4984-8132-b5993911f179.png" align="right" width="134px"/>

[![CRAN](https://img.shields.io/cran/v/testex.svg)](https://cran.r-project.org/package=testex)
[![`R CMD check`](https://github.com/dgkf/testex/workflows/R-CMD-check/badge.svg)](https://github.com/dgkf/testex/actions?query=workflow%3AR-CMD-check)
[![coverage](https://img.shields.io/codecov/c/github/dgkf/testex/main.svg)](https://app.codecov.io/gh/dgkf/testex) 

Add tests and assertions in-line in examples

## Quick Start

Set up your package to use `testex` using

```r
testex::use_testex()
```

and then start adding tests!

```r
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

If you were already using `testthat`, you'll immediately see a new test 
context for testing your examples. And if you aren't using `testthat`, then
you'll find that your tests are being run with your examples when you run
`R CMD check`

## Details

`testex` provides two new `roxygen2` tags: `@test` and `@testthat`. 

 - `@test` will check that the result of the last example is identical to 
   your test. You can use the example output in a function using a `.`.

  ```r
  #' @examples
  #' sum(1:10)
  #' @test 55
  #' @test is.numeric(.)
  ```

 - `@testthat` is similar, but has the added benefit of automatically inserting
   a `.` into `testthat::expect_*` functions.

  ```r
  #' @examples
  #' sum(1:10)
  #' @testthat expect_equal(55)
  #' @testthat expect_vector(numeric())
  ```

## Setup

To make the most of `testex`, there are a few configuration steps you might
consider. These are made simple by using:

```r
testex::use_testex()
```

which will :

- [x] Add `packages = "testex"` to the `Roxygen` field in `DESCRIPTION`,
  allowing `roxygen2` to make use of the `testex` tags.
- [x] Add `testex` as a `Suggests` dependency
- [x] Add settings to the `Config/testex/options` field in `DESCRIPTION`,
  enabling example tests during `R CMD check` by default.
- [x] Add a `test-testex.R` test file if you're using `testthat`, 
  enabling example tests during `testthat` test evaluation.

### Running tests with `testthat`

Running tests using `testthat` is simple. Just use

```r
testex::use_testex_as_testthat()
```

This will add a simple, one-line file to your `tests/testthat` directory 
containing

```r
testex::test_examples_as_testthat()
```

By adding this single line to a `testthat` test file (such as 
`tests/testthat/test-testex.R`), your example tests will be included as part
of your test suite.

When run this way, `testex` tests are embedded with additional metadata
including the original file location of the examples so that `testthat` is 
able to provide more informative error messages.

### Disabling example checks during `R CMD check`

By default, your tests will run when your run examples using `R CMD check`.
However, `R CMD check` will stop on the first error and truncates error output,
which can be inconvenient for debugging. If you'd prefer not to run tests 
during checking, you can add the following line to your `DESCRIPTION`.

```
Config/testex/options: list(check = FALSE)
```

## Goals

R offers some pretty outstanding tools for presenting example code alongside a
package. `testex` aims to make it a bit more powerful by giving you a convenient
shorthand for writing tests and easier ability to customize how they are
executed.

* Example tests that integrate nicely with popular tools. For those that like to
  write Rd files by hand and those that like to generate documentation via
  `roxygen2`.

* Choose where and how your examples are tested - whether you prefer to test
  through `R CMD check` or other mechanisms.

### Use Cases

Since R packages may mix and match tools (`roxygen2`, `testthat`, etc.) to suit
their development needs, a mechanism of testing examples should also be flexible
enough to accommodate any development style. Priority workflows include

* Handwritten Rd documentation  
  Probably the least common use-case, but a priority none-the-less since it
  stress tests the goal of delivering a workflow-agnostic testing pattern.

* `roxygen2`-generated Rd documentation  
  It's hard to find a recent package that doesn't use `roxygen2` for generating
  documentation. As such, any tools should integrate nicely.

* `testthat` testing  
  Choose whether you want to treat example tests as tests during `R CMD check`s
  of examples or during your additional testing steps. Use `testthat` to
  execute example tests and integrate the results with your test suite.

All paths lead back to `Rd` files as a common ground. Whatever tool you prefer
to use, using this central format as the basis for testing means that `testex`
can accommodate your workflow.

## Under the hood

Before we jump into what `testex` offers, let's take a look at the underlying
tools that it builds on top of. 

### Base building-blocks

If you'd like to add testing code to examples in your documentation, you can use
the `\testonly` block to add code which only executes when testing examples.
Pretty neat!

```latex
\examples{
identity("hello, world")
\testonly{
  stopifnot(.Last.value == "hello, world")
}
}
```

Here we use `.Last.value` to grab the result of our last example and test it
against an expected value. Though, as you might expect, you can't easily add
_another_ test because `.Last.value` will have changed. This is where `testex`
comes in to help simplify things.

```latex
\examples{
identity("hello, world")
\testonly{testex::testex(
  is.character(.),
  . == "hello, world")
)}
}
```

Already `testex` is doing a bit of work to make our lives easier. The
`.Last.value` is propagated to each of the tests and we can use the convenient
shorthand `.` to refer to the value we want to test.

### Use a `roxygen2` tag!

If you're already using `roxygen2`, then things get even easier! `roxygen2` 
can make use of new tags provided by `testex`:

```r
#' Hello, World!
#' 
#' @examples
#' 
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

After running `roxygen2::roxygenize()`, you can take a peak at the `Rd` files
and see how the code has been translated to `testex` tests.

### Leverage `testthat` expectations

A convenience tag is also provide for those that prefer the `testthat` style of
testing. `testthat` provides a wealth of expectation functions, which can be used
in conjunction with `testex` to write more familiar tests. 

```r
#' Hello, World!
#' 
#' @examples
#' 
#' hello("World")
#' @testthat expect_equal("Hello, World!")
#'
#' hello("testthat my old friend")
#' @testthat expect_match("testthat")
#' 
#' @export
hello <- function(who) {
  paste0("Hello, ", who, "!")
}
```

The `@testthat` tag will automatically insert the `.Last.value` from the
previous example into the first argument of each expectation. Multiple
consecutive `@testthat` expectations will all test the previous example output.

## Planned Features

|   |   |
|---|---|
| Example result propagation using `testex::testex()`| :ballot_box_with_check: |
| `DESCRPTION` `Config/testex/options` to disable execution during `R CMD check` | :ballot_box_with_check: |
| `roxygen2` tag `@test` | :ballot_box_with_check: |
| `roxygen2` tag `@testthat` | :ballot_box_with_check: |
| Aggregation with `testthat` test results | :ballot_box_with_check: |
| Add stable Rd-file API* | :thought_balloon: |
| Other ideas? Request a feature! | :thought_balloon: |
| Have a better name for the package? I'm all ears! | :ear: |

## Prior Art

I stumbled across the awesome [`roxytest`](https://github.com/mikldk/roxytest)
package when searching around for a solution like this. That package takes a bit
of a different approach, embedding test _writing_ alongside examples. The test
tags that are offered in that package are used to generate `tinytest` and
`testthat` tests from in-line tests but otherwise tests are effectively
independent from the code that goes on to live in an example.

For me, this didn't quite solve the problem of ensuring that package examples
continue to function as expected.  I wanted to explore whether the code in the
examples could be tested _directly_ with more extensive integration with the
existing `examples` tools to decouple the testing from `roxygen2` (or any
testing suite) specifically.
