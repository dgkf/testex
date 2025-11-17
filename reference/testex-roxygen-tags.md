# [`testex`](testex.md) `roxygen2` tags

[`testex`](testex.md) provides two new `roxygen2` tags, `@test` and
`@testthat`.

## tags

[testex](testex.md) tags are all sub-tags meant to be used within an
`@examples` block. They should be considered as tags *within* the
`@examples` block and used to construct blocks of testing code within
example code.

- `@test`: :

  In-line expectations to test the output of the previous command within
  an example. If `.` is used within the test expression, it will be used
  to refer to the output of the previous example command. Otherwise, the
  result of the expression is expected to be identical to the previous
  output.

      #' @examples
      #' 1 + 2
      #' @test 3
      #' @test . == 3
      #'
      #' @examples
      #' 3 + 4
      #' @test identical(., 7)

&nbsp;

- `@testthat`: :

  Similar to `@test`, `@testthat` can be used to make in-line assertions
  using `testthat` expectations. `testthat` expectations follow a
  convention where the first argument is an object to compare against an
  expected value or characteristic. Since the value will always be the
  result of the previous example, this part of the code is implicitly
  constructed for you.

  If you want to use the example result elsewhere in your expectation,
  you can refer to it with a `.`. When used in this way,
  [testex](testex.md) will not do any further implicit modification of
  your expectation.

      #' @examples
      #' 1 + 2
      #' @testthat expect_equal(3)
      #' @testthat expect_gt(0)
      #'
      #' @examples
      #' 3 + 4
      #' @testthat expect_equal(., 7)
