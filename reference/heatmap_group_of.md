# Resolve the heatmap group of each taxon

Looks up each name in `phyto_groups` (trying the exact name first, then
with any trailing sflag such as "spp." stripped, since heatmap row names
carry the suffix while `phyto_groups$name` is the bare WoRMS name).
Unmatched taxa become "Other".

## Usage

``` r
heatmap_group_of(scientific_names, phyto_groups)
```

## Arguments

- scientific_names:

  Character vector of taxon names.

- phyto_groups:

  Data frame with columns `name` and `phyto_group`.

## Value

Character vector of group names, one per input name.

## Details

Unlike the pie charts, the heatmap shows all ciliates as one block, so
the pie-chart-only group `"Mesodinium spp."` is folded into `"Ciliates"`
here.
