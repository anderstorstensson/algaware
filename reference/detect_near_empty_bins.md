# Detect near-empty bins (possible cleaning-cycle samples)

At the end of a cruise the IFCB runs a cleaning cycle in which it pulls
distilled water. When that coincides with a station on the AlgAware
list, the resulting bins contain almost no images and should usually be
excluded from the report. A bin is flagged when its image count is below
the absolute threshold `min_images`. The threshold is deliberately
strict: legitimate bins can be small (e.g. few cells on the west coast
while the Baltic blooms), so a relative/median-based criterion would
produce false positives. Only truly near-empty distilled-water bins
should be caught.

## Usage

``` r
detect_near_empty_bins(matched, min_images = 20)
```

## Arguments

- matched:

  Data frame of station-matched metadata. Requires `pid` and `n_images`
  columns; `STATION_NAME` is used when present.

- min_images:

  Absolute threshold: bins with fewer images are flagged.

## Value

Data frame with `pid`, `STATION_NAME`, and `n_images` for flagged bins,
sorted by image count (ascending). Empty when nothing is flagged or when
no usable image counts exist.
