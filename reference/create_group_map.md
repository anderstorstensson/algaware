# Create a phytoplankton group composition map

Thin AlgAware-specific wrapper around
[`create_pie_map`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_pie_map.md).
Draws a pie chart at each station showing the relative carbon biomass
contributed by Diatoms, Dinoflagellates, Cyanobacteria, and Other.

## Usage

``` r
create_group_map(station_summary, phyto_groups, r_lat = 0.28)
```

## Arguments

- station_summary:

  Aggregated station data from
  [`aggregate_station_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/aggregate_station_data.md),
  containing columns `name`, `AphiaID`, `carbon_ug_per_liter`,
  `STATION_NAME_SHORT`, `LATITUDE_WGS84_SWEREF99_DD`, and
  `LONGITUDE_WGS84_SWEREF99_DD`.

- phyto_groups:

  Data frame with columns `name`, `AphiaID`, and `phyto_group` as
  returned by
  [`SHARK4R::assign_phytoplankton_group()`](https://sharksmhi.github.io/SHARK4R/reference/assign_phytoplankton_group.html).

- r_lat:

  Pie chart radius in latitude degrees (default `0.28`).

## Value

A ggplot object.
