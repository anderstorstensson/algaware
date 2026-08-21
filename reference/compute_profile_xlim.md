# Compute the shared x-axis limit for CTD fluorescence profiles

Returns a fixed 0–10 µg/L scale unless any chlorophyll value (CTD
fluorescence or same-cruise bottle CPHL, 0–50 m) across the *entire*
dataset exceeds 10 µg/L. In that unusual case the scale becomes dynamic
(5\\ and because it is computed from the full dataset every region
figure inherits the same limits.

## Usage

``` r
compute_profile_xlim(ctd_data_full, lims_data_full = NULL)
```

## Arguments

- ctd_data_full:

  Data frame from
  [`read_cnv_folder_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder_all.md)
  covering all regions.

- lims_data_full:

  Data frame from
  [`read_lims_data_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data_all.md),
  or NULL.

## Value

Numeric length-2 vector of x-axis limits.
