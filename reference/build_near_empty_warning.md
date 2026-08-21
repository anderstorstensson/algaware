# Build the persistent near-empty-bin warning UI

Build the persistent near-empty-bin warning UI

## Usage

``` r
build_near_empty_warning(near_empty, max_listed = 10)
```

## Arguments

- near_empty:

  Data frame from
  [`detect_near_empty_bins()`](https://nodc-sweden.github.io/ifcb-algaware/reference/detect_near_empty_bins.md).

- max_listed:

  Maximum number of bins to list individually; the rest are summarized
  as "+ n more".

## Value

A
[`shiny::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html)
for use in `showNotification()`.
