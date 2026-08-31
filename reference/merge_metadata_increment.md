# Merge an increment into cached metadata

Cached rows sampled on or after `start_date` are replaced by the freshly
fetched rows for that window, so same-day additions and edits (e.g. skip
flags or a cruise number assigned later that day) are picked up. Rows
without a parseable sample time are kept.

## Usage

``` r
merge_metadata_increment(cached, fresh, start_date)
```

## Arguments

- cached:

  Cached metadata data.frame.

- fresh:

  Increment data.frame.

- start_date:

  First date covered by `fresh`.

## Value

Combined data.frame.
