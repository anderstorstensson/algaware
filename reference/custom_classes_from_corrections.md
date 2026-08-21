# Reconstruct custom classes from an imported corrections data frame

Inverse of
[`enrich_corrections_for_export()`](https://nodc-sweden.github.io/ifcb-algaware/reference/enrich_corrections_for_export.md):
extracts the custom classes embedded in a corrections CSV so they can be
re-added on import. Only classes not already in `known_classes` are
returned.

## Usage

``` r
custom_classes_from_corrections(df, known_classes)
```

## Arguments

- df:

  Imported corrections data frame.

- known_classes:

  Character vector of already-known class names (database class list,
  taxa lookup, and existing custom classes).

## Value

A data.frame in the shape of `rv$custom_classes` (possibly zero rows).

## Details

Backwards compatible with files written before `custom_is_diatom`
existed: a missing column (or `NA` values) defaults `is_diatom` to
`FALSE`, matching the old import behaviour.
