# Test a list of files

Test a list of files

## Usage

``` r
test_files(files, context, reporter, ...)
```

## Arguments

- files:

  A collection of file paths to test

- context:

  An optional context message to display in `testthat` reporters

- reporter:

  A reporter to use when running each test file

- ...:

  Additional arguments passed to `testhat::source_file`

## Value

The result of
[`testthat::source_file()`](https://testthat.r-lib.org/reference/source_file.html),
after iterating over generated test files.
