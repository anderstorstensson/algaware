# Resolve per-class diatom status, reusing cached lookups

Resolution order per class: the curated `is_diatom` column of the taxa
lookup (no network), then a previously resolved status table, then a
WoRMS lookup for classes neither source covers (e.g. classifier classes
added after the bundled lookup was last updated). Classes whose earlier
WoRMS lookup failed (NA) are retried. The local diatom genus list
([`identify_diatom_classes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/identify_diatom_classes.md))
is NOT applied here – it can change within a session as custom classes
are added, so it is applied at summary time instead.

## Usage

``` r
resolve_diatom_status(class_names, cached_status = NULL, taxa_lookup = NULL)
```

## Arguments

- class_names:

  Character vector of class names in use.

- cached_status:

  Optional data.frame from a previous call, with `class` and
  `worms_diatom` columns.

- taxa_lookup:

  Optional taxa lookup data.frame; when it has a logical `is_diatom`
  column, classes with a non-NA flag are resolved from it without any
  WoRMS lookup.

## Value

A data.frame with `class` and `worms_diatom` (logical, NA when the
lookup failed) covering all `class_names`.
