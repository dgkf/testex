# Convert a [`srcref`](https://rdrr.io/r/base/srcfile.html) to a [`character`](https://rdrr.io/r/base/character.html) representation

Convert a [`srcref`](https://rdrr.io/r/base/srcfile.html) to a
[`character`](https://rdrr.io/r/base/character.html) representation

## Usage

``` r
srcref_key(x, nloc = 2, path = c("base", "root", "full"))
```

## Arguments

- x:

  A [`srcref`](https://rdrr.io/r/base/srcfile.html) object

- nloc:

  The number of locations
  ([`utils::getSrcLocation`](https://rdrr.io/r/utils/sourceutils.html))
  to use. Defaults to 2, indicating starting and ending line number.

- path:

  A form of file path to use for the key. One of `"base"` for only the
  basename of the source file path, `"root"` for a path relative to a
  package root directory if found, or `"full"` for the full file path.

## Value

A string hash of a [srcref](https://rdrr.io/r/base/srcfile.html)
