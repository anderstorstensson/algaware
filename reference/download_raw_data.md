# Download raw IFCB files for selected bins

Downloads .roi, .adc, and .hdr files to local storage. Skips files that
already exist. Chunk size and inter-chunk delay are tunable via
[`options()`](https://rdrr.io/r/base/options.html) (see
[`download_tuning()`](https://nodc-sweden.github.io/ifcb-algaware/reference/download_tuning.md)).

## Usage

``` r
download_raw_data(
  dashboard_url,
  sample_ids,
  dest_dir,
  progress_callback = NULL
)
```

## Arguments

- dashboard_url:

  Dashboard base URL.

- sample_ids:

  Character vector of sample PIDs.

- dest_dir:

  Destination directory.

- progress_callback:

  Optional function(current, total, message) for progress updates.

## Value

Invisible NULL.
