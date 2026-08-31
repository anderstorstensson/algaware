# Look up diatom status in WoRMS

Thin wrapper around
[`iRfcb::ifcb_is_diatom()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_is_diatom.html)
so tests can mock the network call. Returns NA for every class when the
lookup fails (e.g. offline), so failed classes are retried on the next
call rather than being permanently cached as non-diatoms.

## Usage

``` r
worms_diatom_lookup(class_names)
```

## Arguments

- class_names:

  Character vector of class names.

## Value

Logical vector (TRUE/FALSE from WoRMS, NA on lookup failure).
