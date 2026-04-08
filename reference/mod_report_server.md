# Report Module Server

Report Module Server

## Usage

``` r
mod_report_server(id, rv, config, phyto_groups_reactive = NULL)
```

## Arguments

- id:

  Module namespace ID.

- rv:

  Reactive values for app state.

- config:

  Reactive values with settings.

- phyto_groups_reactive:

  Optional reactive returning a data frame of phytoplankton group
  assignments (`name`, `AphiaID`, `phyto_group`). When supplied, the
  cached value is forwarded to
  [`generate_report()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_report.md)
  so the WoRMS lookup is not repeated.

## Value

NULL (side effects only).
