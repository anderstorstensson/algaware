# Build the prompts for a station description

The prompt-building half of
[`generate_station_description()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_station_description.md),
exposed separately so
[`add_station_sections()`](https://nodc-sweden.github.io/ifcb-algaware/reference/add_station_sections.md)
can build all station prompts up front and perform the LLM requests in
parallel
([`call_llm_batch()`](https://nodc-sweden.github.io/ifcb-algaware/reference/call_llm_batch.md)).

## Usage

``` r
build_station_description_prompts(
  station_data,
  taxa_lookup = NULL,
  all_stations_summary = NULL,
  phyto_groups = NULL,
  unclassified_pct = NULL,
  chl_measure = "fluorescence"
)
```

## Arguments

- station_data:

  Data frame with station_summary rows for one visit.

- taxa_lookup:

  Optional taxa lookup table.

- all_stations_summary:

  Optional full station_summary for context.

- phyto_groups:

  Optional phytoplankton group table used to provide explicit group
  assignments in the prompt text.

- unclassified_pct:

  Optional per-class unclassified percentage info used for context.

- chl_measure:

  How the active chlorophyll source is measured, `"fluorescence"`
  (FerryBox/CTD) or `"concentration"` (LIMS bottle/hose filter samples).
  Adjusts the chlorophyll terminology used.

## Value

A list with `system` and `user` prompt strings.
