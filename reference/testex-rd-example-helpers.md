# Rd Example Parsing Helpers

Rd Example Parsing Helpers

## Usage

``` r
rd_extract_examples(rd)

rd_code_as_string(rd)

split_testonly_as_expr(rd)

split_testonly(rd)
```

## Arguments

- rd:

  An Rd object

## Value

The examples section of an Rd object

A formatted Rd example

An interlaced list of expressions, either representing example code or
tests. The names of the list are either `\testonly` or `RDCODE`
depending on the originating source of the expression.

A list of Rd tag contents

## Functions

- `rd_extract_examples()`: Extract examples tag from an Rd file

- `rd_code_as_string()`: Convert an Rd example to string

- `split_testonly_as_expr()`: Split sections of an example into
  evaluated example code blocks and code blocks wrapped in `\testonly`
  `Rd_tag`s, reassigning
  [`srcref`](https://rdrr.io/r/base/srcfile.html)s as the example code
  is split.

- `split_testonly()`: Split sections of an example into lists of
  `Rd_tag`s. Note that [`srcref`](https://rdrr.io/r/base/srcfile.html)s
  are split by line number. If a line is split between two sections, it
  is attributed to the first section. As this is used primarily for
  giving line numbers to test messages, this is sufficient for providing
  test failures locations.
