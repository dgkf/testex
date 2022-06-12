# `testex` (experimental :construction_worker:)

Add tests and assertions in-line in examples

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

## Quick Start

### 1. Add some expectations

Adding tests right next to your examples using the roxygen `@expect` tag.

```r
#' Hello, World!
#' 
#' @examples
#' hello("World")
#' @expect "Hello, World!"
#'
#' hello("darkness my old friend")
#' @expect grepl("darkness", .)
#' 
#' @export
hello <- function(who) {
  paste0("Hello, ", who, "!")
}
```

### 2. Add the `roclet`

To enable this roclet, you'll also need to modify your package's `DESCRIPTION`
to include the `testex::rd` roclet. Adding it is as easy as calling:

```r
testex::use_rd_roclet()
```

This will modify your existing roclets, replacing the default `roxygen2` `"rd"`
roclet with `testex`'s:

```diff
- Roxygen: list(markdown = TRUE, roclets = c("namespace", "rd"))
+ Roxygen: list(markdown = TRUE, roclets = c("namespace", "testex::rd"))
```

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
`.Last.value` is propegated to each of the tests and we can use the convenient
shorthand `.` to refer to the value we want to test.

### Use a `roclet`!

If you're already using `roxygen2`, then things get even easier! You can add in
the `"testex::rd"` roclet (replacing the default `roxygen2` `"rd"` roclet) and
make use of the `@expect` tag.

```r
#' Hello, World!
#' 
#' @examples
#' 
#' hello("World")
#' @expect "Hello, World!"
#'
#' hello("darkness my old friend")
#' @expect grepl("darkness", .)
#' 
#' @export
hello <- function(who) {
  paste0("Hello, ", who, "!")
}
```

After running `roxygen2::roxygenize()`, you can take a peak at the `Rd` files
and see how the code has been translated to `testex` tests.

### Leverage `testthat` expecations

A convenience tag is also provided for those that prefer the `testthat` style of
testing. `testthat` provides a wealth of expecation functions, which can be used
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
previous example into the first argument of each expectation. 

## Roadmap

|   |   |
|---|---|
| Example result propegation using `testex::testex()`| :ballot_box_with_check: |
| `DESCRPTION` `Config/testex/options` to disable execution during `R CMD check` | :ballot_box_with_check: |
| `DESCRPTION` `Config/testex/options` to enable execution from script in `/tests` | :black_square_button: |
| `roxygen2` tag `@expect` | :ballot_box_with_check: |
| `roxygen2` tag `@testthat` | :ballot_box_with_check: |
| Aggregation with `testthat` test results | :black_square_button: |
| Other ideas? Request a feature! | :thought_balloon: |
| Have a better name for the package. I'm all ears! | :ear: |

## Prior Art

I stumbled across the aweseome [`roxytest`](https://github.com/mikldk/roxytest)
package when searching around for a solution like this. That package takes a bit
of a different approach, embedding test _writing_ alongside examples. The test
tags that are offered in that package are used to generate `tinytest` and
`testthat` tests from in-line tests but otherwise tests are effectively
independent from the code that goes on to live in an example.

For me, this didn't quite solve the problem of ensuring that package exampes
continue to function as expected.  I wanted to explore whether the code in the
examples could be tested _directly_ with more extensive integration with the
existing `examples` tools to decouple the testing from `roxygen2` (or any
testing suite) specifically.
