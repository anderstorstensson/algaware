# Null coalescing operator

Base R only ships `%||%` from 4.4.0, while this package supports R \>=
4.1. It must also be exported (not just defined internally): the Shiny
app in `inst/app/` resolves names through the attached namespace, so an
internal definition would leave `server.R` without it on older R
versions.

## Usage

``` r
x %||% y
```

## Arguments

- x, y:

  Any objects.

## Value

`x` unless it is `NULL`, in which case `y`.
