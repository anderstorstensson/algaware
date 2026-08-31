# Build the per-ROI biovolume cache from downloaded files

Reads all feature CSVs and .hdr files for the given samples once. The
raw pixel biovolume is stored (not micron-converted) so a later change
to the pixels-per-micron setting still takes effect at summary time.

## Usage

``` r
build_biovolume_cache(feature_folder, hdr_folder, sample_ids)
```

## Arguments

- feature_folder:

  Path to the feature CSV directory.

- hdr_folder:

  Path to the raw data directory (for .hdr/.adc files).

- sample_ids:

  Character vector of sample IDs to include.

## Value

A list with `roi_biovolumes` (data.frame: `sample`, `roi_number`,
`biovolume_px`) and `sample_volumes` (data.frame: `sample`,
`ml_analyzed`).
