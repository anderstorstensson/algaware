# Add mosaic section to the report

The mosaic counter and the "Image mosaics" heading are threaded through
the per-region calls (like the figure counter elsewhere) so the report
gets one heading and a continuous "Mosaic N." numbering, instead of a
duplicate heading-2 block and two figures both captioned "Mosaic 1.".

## Usage

``` r
add_mosaic_section(
  doc,
  mosaics,
  hab_species,
  region_label,
  cleanup,
  taxa_lookup = NULL,
  mosaic_num = 1L,
  add_heading = TRUE
)
```

## Value

A list with `doc`, the next `mosaic_num`, and `heading_added` (whether
this call emitted the section heading).
