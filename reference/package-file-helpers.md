# Package source file helpers

Discover specific package related file paths

## Usage

``` r
find_package_root(path = ".", quiet = FALSE)

find_package_rds(package, path = getwd())

package_desc(path = getwd())
```

## Arguments

- path:

  A file path within a package's source code or installation directory.
  Only considered if `package` is missing.

- quiet:

  Whether to suppress output

- package:

  A package name

## Value

NULL, invisibly

A list of package Rd objects, as returned by
[`tools::Rd_db()`](https://rdrr.io/r/tools/Rdutils.html)
