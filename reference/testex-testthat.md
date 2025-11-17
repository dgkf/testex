# Support for `testthat` Expectations

`testthat` support is managed through a "style" provided to
[`testex`](testex.md). When using the `testthat` style (automatically
when using the `@testthat` tag), expectations are processed such that
they always refer to the previous example. Special care is taken to
manage propagation of this value through your test code, regardless of
how `testthat` is executed.

## Examples

``` r
# example code
1 + 2
#> [1] 3

# within `testex` block, test code refers to previous result with `.`
testex(style = "testthat", srcref = "abc.R:1:3", { DONTSHOW({
  . <- 3 # needed because roxygen2 @examplesIf mutates .Last.value
  })
  test_that("addition holds up", {
    expect_equal(., 3)
  })
})
#> Error in DONTSHOW({    . <- 3}): could not find function "DONTSHOW"
```
