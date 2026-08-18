# Normalise extra_stations to a list of per-station lists

Settings files written while the data.frame read bug was live may have
been re-saved in a mangled shape; accept both a data.frame and a list of
lists so those installations recover on the next load.

## Usage

``` r
normalize_extra_stations(x)
```

## Arguments

- x:

  The `extra_stations` value read from settings.

## Value

A list of station lists (possibly empty).
