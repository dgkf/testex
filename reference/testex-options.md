# Cached retrieval of testex options from package DESCRIPTION

As long as the `fingerprint` has not changed, the package `DESCRIPTION`
will be read only once to parse and retrieve configuration options. If
the `DESCRIPTION` file is modified or if run from a separate process,
the configured settings will be refreshed based on the most recent
version of the file.

## Usage

``` r
memoise_testex_desc(path, fingerprint, ...)

testex_options(path = package_desc(), ...)
```

## Arguments

- path:

  A path in which to search for a package `DESCRIPTION`

- fingerprint:

  An object used to indicate when the cached values have been
  invalidated

## Value

The test options environment, invisibly.

The test options environment as a list

## Functions

- `testex_options()`:
