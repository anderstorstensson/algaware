# Get the coastline polygon for Swedish waters

Loads the Natural Earth countries layer once per session and crops it to
a bounding box slightly larger than the plot extent, so geom_sf renders
a handful of coastline polygons instead of the whole world on every map.
Cropping failures fall back to the uncropped layer.

## Usage

``` r
sweden_coastline()
```

## Value

An sf object with the cropped coastline.
