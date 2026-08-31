# Read per-sample analysed volumes from .hdr files

Read per-sample analysed volumes from .hdr files

## Usage

``` r
read_sample_volumes(hdr_folder, sample_ids)
```

## Arguments

- hdr_folder:

  Directory containing .hdr (and .adc) files.

- sample_ids:

  Character vector of sample IDs to read.

## Value

A data.frame with `sample` and `ml_analyzed` columns.
