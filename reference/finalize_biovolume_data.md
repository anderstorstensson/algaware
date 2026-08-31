# Join taxonomy onto biovolume data and drop non-biological classes

Shared tail of
[`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md)
and
[`summarize_biovolumes_cached()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes_cached.md):
joins the taxa lookup, fills missing names/flags, and removes
non-biological classes.

## Usage

``` r
finalize_biovolume_data(
  biovolume_data,
  taxa_lookup,
  non_bio_classes = character(0)
)
```

## Arguments

- biovolume_data:

  Per-sample, per-class biovolume data.frame with a `class` column.

- taxa_lookup:

  A data.frame with columns `clean_names`, `name`, `AphiaID` (and
  optionally `sflag`).

- non_bio_classes:

  Character vector of non-biological class names to exclude.

## Value

The joined and filtered data.frame.
