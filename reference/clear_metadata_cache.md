# Delete the metadata cache

Used by the "Clear Metadata Cache" button in Settings: with the cache
gone, the next Fetch Metadata downloads the complete export again. This
is the escape hatch for edits to old bins (e.g. skip flags or cruise
numbers changed on the dashboard) that the incremental fetch cannot see.

## Usage

``` r
clear_metadata_cache(storage_dir)
```

## Arguments

- storage_dir:

  Local storage base directory.

## Value

Invisible logical: TRUE when a cache file existed and was removed.
