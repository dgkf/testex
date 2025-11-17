# Build a source location from a minimal numeric vector

Build a length four source location from a length two source location.
The starting column on the first line is assumed to be 1, and the final
column is taken to be the length of the line if the source file exists,
or 1 as a fallback.

## Usage

``` r
srclocs(x, file)
```

## Arguments

- x:

  A numeric vector of at least length 2

- file:

  A file to use to determine the length of the final line

## Value

A numeric vector similar to a
[`utils::getSrcLocation`](https://rdrr.io/r/utils/sourceutils.html)
object
