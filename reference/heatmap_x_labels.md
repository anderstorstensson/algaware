# Build heatmap x-axis labels from station_date keys

Build heatmap x-axis labels from station_date keys

## Usage

``` r
heatmap_x_labels(x, sample_counts = NULL, compact = FALSE)
```

## Arguments

- x:

  Character vector of `"STATION_YYYY-MM-DD"` keys.

- sample_counts:

  Optional named integer vector of sample counts keyed by `x`.

- compact:

  If `TRUE`, two lines: `"STATION (n = X)"` over the date (for vertical
  labels when many visits are shown); otherwise station, date and
  `"n = X"` on three lines.

## Value

Character vector of labels.
