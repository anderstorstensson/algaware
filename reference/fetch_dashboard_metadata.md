# Fetch metadata from the IFCB Dashboard

Wraps
[`iRfcb::ifcb_download_dashboard_metadata()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_download_dashboard_metadata.html)
and extracts available cruise numbers. When `cache_dir` is given, the
download is cached there as an RDS file and subsequent fetches only
download bins sampled on or after the newest cached day (see
`R/metadata_cache.R`), which makes refetching a large archive a matter
of seconds.

## Usage

``` r
fetch_dashboard_metadata(
  dashboard_url,
  dataset_name = NULL,
  cache_dir = NULL,
  force_full = FALSE
)
```

## Arguments

- dashboard_url:

  Dashboard base URL.

- dataset_name:

  Dataset name (e.g. "RV_Svea").

- cache_dir:

  Optional local storage directory for the metadata cache. NULL
  (default) disables caching and always downloads the full export.

- force_full:

  If TRUE, ignore any cache and download the full export (the cache is
  still refreshed afterwards).

## Value

A list with `metadata` (data.frame), `cruise_numbers` (character vector,
possibly empty if no cruise column exists), `incremental` (logical; TRUE
when a cached fetch was updated incrementally), and `n_new` (number of
bins added or refreshed).
