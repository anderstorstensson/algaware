# Extract IFCB PNGs, skipping ROIs already extracted

Extraction output is deterministic per (sample, ROI, scale settings),
and the extraction folders live in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html), which persists for
the R process. Skipping ROIs whose PNG already exists makes repeated
report or mosaic generation in the same session near-instant. A marker
file records the scale-bar settings; when they change (e.g. the
pixels-per-micron setting was edited), the cached PNGs are discarded and
re-extracted.

## Usage

``` r
extract_pngs_cached(
  roi_file,
  out_folder,
  roi_numbers,
  scale_bar_um = 5,
  scale_micron_factor = NULL
)
```

## Arguments

- roi_file:

  Path to a .roi file.

- out_folder:

  Output directory for extracted PNGs.

- roi_numbers:

  Integer ROI numbers to extract.

- scale_bar_um:

  Scale bar length in microns. Default 5.

- scale_micron_factor:

  Optional microns-per-pixel factor.

## Value

TRUE if all requested PNGs were already cached or an extraction attempt
completed without error, FALSE otherwise.
