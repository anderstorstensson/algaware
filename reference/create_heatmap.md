# Create a heatmap of biovolume by species and station

HAB species are marked with a red asterisk (\*) on the y-axis labels.
When `phyto_groups` is supplied, rows are split into one panel per
phytoplankton group with a coloured group label on the left (colours
match the pie-chart group map).

## Usage

``` r
create_heatmap(
  wide_summary,
  taxa_lookup = NULL,
  title = "",
  sample_counts = NULL,
  phyto_groups = NULL
)
```

## Arguments

- wide_summary:

  Wide-format data from
  [`create_wide_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_wide_summary.md).

- taxa_lookup:

  Optional taxa lookup table with `HAB` column. If provided, HAB species
  are annotated with a red asterisk on the y-axis.

- title:

  Plot title.

- sample_counts:

  Optional named integer vector mapping station_date column names to
  number of samples. If provided, `n = X` is appended to each x-axis
  label.

- phyto_groups:

  Optional data frame with columns `name` and `phyto_group` (as built
  from
  [`assign_phyto_groups()`](https://nodc-sweden.github.io/ifcb-algaware/reference/assign_phyto_groups.md)).
  If provided, taxa are grouped into labelled panels by phytoplankton
  group and ordered alphabetically within each group; otherwise by total
  biovolume (descending).

## Value

A ggplot object.
