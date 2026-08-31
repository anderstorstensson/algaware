# Save dashboard metadata to the cache

Failures are downgraded to a warning: a broken cache write must never
break the fetch itself.

## Usage

``` r
save_metadata_cache(cache_file, dashboard_url, dataset_name, metadata)
```

## Arguments

- cache_file:

  Path from
  [`metadata_cache_path()`](https://nodc-sweden.github.io/ifcb-algaware/reference/metadata_cache_path.md).

- dashboard_url:

  Dashboard base URL.

- dataset_name:

  Dataset name.

- metadata:

  Metadata data.frame to store.

## Value

Invisible NULL.
