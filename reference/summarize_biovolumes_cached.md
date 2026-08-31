# Summarize biovolumes from the in-memory cache

Reproduces the output of
[`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md)
(which wraps
[`iRfcb::ifcb_summarize_biovolumes()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_summarize_biovolumes.html))
without touching the disk: per-ROI biovolumes are joined with the
(possibly corrected) classifications, carbon is converted per ROI with
the diatom or non-diatom formula, and the result is aggregated per
sample and class.

## Usage

``` r
summarize_biovolumes_cached(
  cache,
  classifications,
  taxa_lookup,
  non_bio_classes = character(0),
  pixels_per_micron = 2.77,
  diatom_status = NULL,
  custom_classes = NULL
)
```

## Arguments

- cache:

  List from
  [`build_biovolume_cache()`](https://nodc-sweden.github.io/ifcb-algaware/reference/build_biovolume_cache.md).

- classifications:

  Data.frame with `sample_name`, `roi_number`, `class_name` (from
  [`read_h5_classifications()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_h5_classifications.md),
  possibly corrected).

- taxa_lookup:

  Taxa lookup data.frame (see
  [`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md)).

- non_bio_classes:

  Character vector of non-biological classes to exclude.

- pixels_per_micron:

  Conversion factor from pixels to microns.

- diatom_status:

  Optional data.frame from
  [`resolve_diatom_status()`](https://nodc-sweden.github.io/ifcb-algaware/reference/resolve_diatom_status.md);
  resolved on the fly (WoRMS lookup) when NULL.

- custom_classes:

  Optional data frame of custom classes with an `is_diatom` column
  (extends the local diatom genus list).

## Value

A data.frame with the same columns as
[`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md).

## Details

ROIs without a classification row – including all ROIs of samples the
user has excluded – are dropped, matching the file-based path where such
rows either never enter (excluded samples) or are discarded downstream
by the name-based aggregation (NA class).
