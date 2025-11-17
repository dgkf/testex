# Changelog

## testex 0.2.1

CRAN release: 2025-11-12

- Updated internals to be compatible with the next version of `testthat`
  following `v3.2.3`. ([@dgkf](https://github.com/dgkf)
  [\#12](https://github.com/dgkf/testex/issues/12))

## testex 0.2.0

CRAN release: 2024-04-14

### Breaking Changes

> Documentation syntax changes. Documentation will be need to be
> re-`roxygenize`’d or otherwise updated.

- Changes syntax of tests to minimize reliance on `testex` namespace
  consistency across versions. Instead of using
  `testex(with_srcref(..))` and
  `testthat_block(test_that(.., with_srcref(..)))`, both interfaces are
  now handled via [`testex()`](../reference/testex.md) with an added
  `style` parameter:

  ``` r
  testex(style = "testthat", srcref = "fn.R:10:11", { code })
  ```

  This syntax is intended to be more resilient to changes to keep your
  tests from relying too heavily on an unchanging `testex` function
  interface.

### New Features

- Adds configuration (`Config/testex/options`) field `"version"`, which
  is automatically updated when a newer version of `testex` is first
  used.

  This field is checked to decide whether the currently loaded version
  of `testex` is capable of re-running your tests.

  Currently, a conservative approach is taken. If there is a version
  mismatch, `testex` will suggest updating when run independently using
  a testing framework and will disable `testex` testing during
  `R CMD check` to avoid causing downstream test failures as the API
  changes. However, this means that `testex` tests will be ineffective
  if your package is out-of-date with the released `testex` version on
  `CRAN`

  Past a `v1.0.0` release, this behavior will be relaxed to check for a
  compatible major version.

## testex 0.1.0

CRAN release: 2024-04-04

- Initial CRAN submission
