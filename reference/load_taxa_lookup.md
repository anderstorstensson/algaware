# Load the bundled taxa lookup table

Returns the pre-built mapping from classifier class names to WoRMS
scientific names and AphiaIDs, including the curated `is_diatom` flag
that selects the carbon conversion formula (seeded once from WoRMS, with
homonym genera such as *Actinocyclus* corrected manually).

## Usage

``` r
load_taxa_lookup()
```

## Value

A data.frame with columns `clean_names`, `name`, `sflag`, `AphiaID`,
`HAB`, `warning_level`, `italic`, `is_diatom`.
