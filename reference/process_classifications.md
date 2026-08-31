# Process classifications and compute station summaries

Reads H5 classification files, computes biovolume data for each
classified image, aggregates results by station visit, and extracts the
classifier name from the first H5 file. Non-biological classes are
excluded from biovolume but kept in the classification data frame for
gallery display.

## Usage

``` r
process_classifications(
  config,
  dirs,
  sample_ids,
  matched,
  cached_diatom_status = NULL
)
```

## Arguments

- config:

  Reactive values with settings (`non_biological_classes`,
  `pixels_per_micron`).

- dirs:

  List with `raw_dir`, `feat_dir`, `class_dir` paths.

- sample_ids:

  Character vector of sample PIDs to process.

- matched:

  Data frame of station-matched metadata.

- cached_diatom_status:

  Optional data.frame from a previous
  [`resolve_diatom_status()`](https://nodc-sweden.github.io/ifcb-algaware/reference/resolve_diatom_status.md)
  call (e.g. an earlier load in the same session), so already-resolved
  classes skip the WoRMS lookup.

## Value

A named list, or NULL if no H5 classifications were found.
