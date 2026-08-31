# Load cached dashboard metadata

Load cached dashboard metadata

## Usage

``` r
load_metadata_cache(cache_file, dashboard_url, dataset_name)
```

## Arguments

- cache_file:

  Path from
  [`metadata_cache_path()`](https://nodc-sweden.github.io/ifcb-algaware/reference/metadata_cache_path.md).

- dashboard_url:

  Dashboard base URL the cache must match.

- dataset_name:

  Dataset name the cache must match.

## Value

The cached metadata data.frame, or NULL when there is no usable cache
for this URL/dataset.
