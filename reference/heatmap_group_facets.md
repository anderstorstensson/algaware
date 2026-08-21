# Facet layers that draw coloured group strips on the heatmap

One panel per group (rows sized to the number of taxa), with the group
name on the left, coloured with the shared pie-chart palette. Labels are
horizontal (not rotated) so single-row groups cannot clip the text. A
thin grey background band separates the panels. Groups without a palette
entry fall back to the "Other" grey.

## Usage

``` r
heatmap_group_facets(group_levels)
```

## Arguments

- group_levels:

  Character vector of group levels in display order.

## Value

A list of ggplot2 components to add to a plot.
