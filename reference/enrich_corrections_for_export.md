# Enrich a corrections data frame with custom class metadata

Appends columns describing any custom class referenced in the
`new_class` column so that a corrections CSV is self-contained and can
be re-imported to reconstruct custom classes. Rows whose `new_class` is
not in `custom_classes` receive `NA` in all added columns.

## Usage

``` r
enrich_corrections_for_export(corrections, custom_classes)
```

## Arguments

- corrections:

  Data frame with at least a `new_class` column.

- custom_classes:

  Data frame of custom classes (from `rv$custom_classes`).

## Value

`corrections` with extra columns `custom_sci_name`, `custom_sflag`,
`custom_aphia_id`, `custom_hab`, `custom_italic`, `custom_is_diatom`.
