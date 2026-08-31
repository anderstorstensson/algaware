# Derive sample IDs from feature file names

Feature files are named `<pid>_fea_vN.csv` (IFCB convention, matched by
iRfcb) or `<pid>_features.csv`; strip either suffix to recover the bare
sample ID.

## Usage

``` r
feature_sample_name(files)
```

## Arguments

- files:

  Character vector of feature file paths or basenames.

## Value

Character vector of sample IDs.
