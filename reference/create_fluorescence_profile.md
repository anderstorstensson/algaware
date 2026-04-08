# Create a CTD fluorescence profile plot for one station

Plots all casts at the station as separate overlaid paths (one per CNV
file). Depth is clamped to 0–50 m.

## Usage

``` r
create_fluorescence_profile(
  station_ctd,
  station_label,
  xlim = NULL,
  date_label = NULL,
  show_x_axis = TRUE,
  lims_points = NULL
)
```

## Arguments

- station_ctd:

  Data frame for one station with columns `chl_fluorescence`,
  `pressure_dbar`, and `file_path`.

- station_label:

  Character label for the station.

- xlim:

  Numeric length-2 vector for a shared x-axis limit, or NULL.

- date_label:

  Character subtitle (sample dates), or NULL.

- show_x_axis:

  Logical; show x-axis tick labels and title on this panel.

- lims_points:

  Optional data frame with columns `DEPH` and `CPHL` for discrete bottle
  chlorophyll measurements from the current cruise (depth \\\le 50\\ m).
  Points are overlaid on the profile.

## Value

A ggplot object.
