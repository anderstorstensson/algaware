# Order heatmap taxa by phytoplankton group, then alphabetically

Groups follow the canonical order used elsewhere in AlgAware (Diatoms,
Dinoflagellates, Cyanobacteria, Cryptophytes, *Mesodinium* spp.,
Silicoflagellates), then any additional groups alphabetically, with
"Other" always last. Taxa are sorted alphabetically within each group;
taxa missing from `phyto_groups` fall into "Other".

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
