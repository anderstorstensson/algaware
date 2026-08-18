# Assign phytoplankton groups using the bundled YAML configuration

Thin wrapper around
[`SHARK4R::assign_phytoplankton_group()`](https://sharksmhi.github.io/SHARK4R/reference/assign_phytoplankton_group.html)
that reads group definitions from `inst/config/phyto_groups.yaml` so
class/phylum mappings do not need to be hardcoded in application code.

## Usage

``` r
assign_phyto_groups(scientific_names, aphia_ids = NULL, verbose = FALSE)
```

## Arguments

- scientific_names:

  Character vector of scientific names.

- aphia_ids:

  Integer vector of AphiaIDs (same length), or `NULL`.

- verbose:

  Passed to
  [`SHARK4R::assign_phytoplankton_group()`](https://sharksmhi.github.io/SHARK4R/reference/assign_phytoplankton_group.html).

## Value

Character vector of group names, one per element of `scientific_names`;
unresolved taxa are `"Other"`. (The raw
[`SHARK4R::assign_phytoplankton_group()`](https://sharksmhi.github.io/SHARK4R/reference/assign_phytoplankton_group.html)
data frame is not returned: its `left_join` shape leaked into callers as
nested `phyto_group.*` columns and could drop or duplicate rows, so it
is realigned to the input here.)
