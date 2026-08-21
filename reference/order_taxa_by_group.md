# Order heatmap taxa by phytoplankton group, then alphabetically

Groups follow
[`heatmap_group_levels()`](https://nodc-sweden.github.io/ifcb-algaware/reference/heatmap_group_levels.md);
taxa are sorted alphabetically within each group.

## Usage

``` r
order_taxa_by_group(scientific_names, phyto_groups)
```

## Arguments

- scientific_names:

  Character vector of taxon names to order.

- phyto_groups:

  Data frame with columns `name` and `phyto_group`.

## Value

`scientific_names` reordered (top-to-bottom display order).
