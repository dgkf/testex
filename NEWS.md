# testex (development)

> **Pre-`v1.0.0` Lifecycle Policy**
> 
> Be aware that this package produces code that enters into your package's
> R documentation files. Until `testex` reaches `1.0.0`, there are no 
> guarantees for a stable interface, which means your package's tests written
> in documentation files may fail if interface changes.
>

# testex 0.2.0

## Breaking Changes

> Documentation syntax changes. Documentation will be need to be 
> re-`roxygenize`'d or otherwise updated.

* Changes syntax of tests to minimize reliance on `testex` namespace 
  consistency across versions. Instead of using `testex(with_srcref(..))` and
  `testthat_block(test_that(.., with_srcref(..)))`, both interfaces are now
  handled via `testex()` with an added `style` parameter:

  ```r
  testex(style = "testthat", srcref = "fn.R:10:11", { code })
  ```

  This syntax is intended to be more resiliant to changes to keep your
  tests from relying to heavily on an unchanging `testex` function interface.

## New Features

* Adds config (`Config/testex/options`) field `"version"`, which is 
  automatically updated when a newer version of `testex` is first used. 
  
  This field is checked to decide whether the currently loaded version of 
  `testex` is capable of re-running your tests.

  Currently, a conservative approach is taken. If there is a version mismatch,
  `testex` will suggest updating when run independently using a testing 
  framework and will disable `testex` testing during `R CMD check` to avoid
  causing downstream test failures as the API changes. However, this means
  that `testex` tests will be ineffective if your package is out-of-date 
  with the released `testex` version on `CRAN`

  Past a `v1.0.0` release, this behavior will be relaxed to check for a
  compatible major version.
  
# testex 0.1.0

* Initial CRAN submission
