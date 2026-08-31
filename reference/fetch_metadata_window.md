# Fetch dashboard metadata for a date window

Same `export_metadata` endpoint and CSV parsing as
[`iRfcb::ifcb_download_dashboard_metadata()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_download_dashboard_metadata.html)
(readr with all-character columns followed by `type_convert()`), plus
the `start_date`/ `end_date` query parameters the dashboard supports.

## Usage

``` r
fetch_metadata_window(
  dashboard_url,
  dataset_name,
  start_date,
  end_date = Sys.Date() + 1
)
```

## Arguments

- dashboard_url:

  Dashboard base URL.

- dataset_name:

  Dataset name.

- start_date:

  Start date (inclusive).

- end_date:

  End date (inclusive). Defaults to tomorrow so bins sampled today are
  always included.

## Value

Metadata data.frame (possibly zero rows).
