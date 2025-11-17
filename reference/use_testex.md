# Add [`testex`](testex.md) tags and configure package to fully use [`testex`](testex.md) features

Add [`testex`](testex.md) tags and configure package to fully use
[`testex`](testex.md) features

## Usage

``` r
use_testex(path = getwd(), check = TRUE, quiet = FALSE)
```

## Arguments

- path:

  A package source code working directory

- check:

  A `logical` value indicating whether tests should be executing during
  `R CMD check`.

- quiet:

  Whether output should be suppressed

## Value

The result of [`write.dcf()`](https://rdrr.io/r/base/dcf.html) upon
modifying the package `DESCRIPTION` file.

## Note

The [`testex`](testex.md) `roxygen2` tags behave similarly to `roxygen2`
`@examples` tags, with the minor addition of some wrapping code to
manage the tests. This means that they will be integrated into your
`@examples` and can be intermixed between `@examples` tags
