# Bind incremental metadata rows onto cached metadata

Uses the union of both column sets (missing columns become NA) and
coerces increment columns to the cached column types.

## Usage

``` r
bind_metadata_rows(cached, fresh)
```

## Arguments

- cached:

  Cached metadata data.frame.

- fresh:

  Increment data.frame from
  [`fetch_metadata_window()`](https://nodc-sweden.github.io/ifcb-algaware/reference/fetch_metadata_window.md).

## Value

Combined data.frame.
