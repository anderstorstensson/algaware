# Recompute biovolume data, using the session cache when available

Central entry point for summary recomputation (report generation, sample
exclusions, stale-summary refresh). Uses the in-memory per-ROI cache
built at load time; falls back to the file-based
[`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md)
when no cache exists. Updates `rv$diatom_status` with any newly resolved
classes as a side effect.

## Usage

``` r
recompute_biovolume_data(
  rv,
  config,
  taxa_lookup,
  non_bio_classes = character(0)
)
```

## Arguments

- rv:

  Reactive values (or list-like) with `classifications`,
  `custom_classes`, `biovolume_cache`, `diatom_status`.

- config:

  Reactive values (or list-like) with `local_storage_path` and
  `pixels_per_micron`.

- taxa_lookup:

  Taxa lookup data.frame (already merged with custom taxa where
  applicable).

- non_bio_classes:

  Character vector of non-biological classes.

## Value

A data.frame as returned by
[`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md).
