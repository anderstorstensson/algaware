# Identify diatom classes from taxa lookup

Diatoms require a different biovolume formula than other phytoplankton
(they have silica frustules that affect the carbon:biovolume ratio). The
primary source is the curated `is_diatom` column of the taxa lookup
(seeded once from WoRMS); rows without a value – and lookups predating
the column – fall back to matching class names against known diatom
genera. The result feeds the `diatom_include` argument of
[`iRfcb::ifcb_summarize_biovolumes()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_summarize_biovolumes.html)
and the cached summary path.

## Usage

``` r
identify_diatom_classes(taxa_lookup, custom_classes = NULL)
```

## Arguments

- taxa_lookup:

  A data.frame with a `clean_names` column and optionally a logical
  `is_diatom` column.

## Value

Character vector of class names likely to be diatoms.
