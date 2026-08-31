# Read analysed volume for one sample, returning NA on failure

Wraps
[`iRfcb::ifcb_volume_analyzed()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_volume_analyzed.html)
(which needs the .adc file next to the .hdr file) so that one corrupt or
incomplete sample yields an NA volume – and therefore NA per-liter
values downstream – instead of aborting the whole summary.

## Usage

``` r
volume_analyzed_safe(hdr_file)
```

## Arguments

- hdr_file:

  Path to a .hdr file.

## Value

Numeric ml analysed, or NA.
