# Build pie-chart polygon data for use with geom_polygon

Computes arc segments for each group slice at each station. The
longitude radius is stretched by `1/cos(lat)` so that slices appear
circular under `coord_sf` (which equalises physical distances on
screen).

## Usage

``` r
build_pie_polygons(wide_data, group_cols, n_arc = 80)
```

## Arguments

- wide_data:

  Data frame with one row per station, columns `lon`, `lat`, `r_pie`,
  and one numeric column per group.

- group_cols:

  Character vector of column names holding group values.

- n_arc:

  Number of arc points per full circle. Default `80`.

## Value

A data frame with columns `x`, `y`, `slice_id`, and `group` suitable for
`geom_polygon`.
