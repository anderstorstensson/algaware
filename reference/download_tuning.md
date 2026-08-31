# Download tuning parameters

[`iRfcb::ifcb_download_dashboard_data()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_download_dashboard_data.html)
downloads in parallel chunks and sleeps unconditionally after every
chunk (its defaults: 5 files per chunk, 2 s sleep). With the 4 small
files a sample needs, that idle time dominates a first-time cruise load,
so algaware defaults to larger chunks and a much shorter politeness
delay. Override for slow or third-party dashboards via
`options(algaware.download_parallel = , algaware.download_sleep = )`.

## Usage

``` r
download_tuning()
```

## Value

A list with `parallel` and `sleep` values.
