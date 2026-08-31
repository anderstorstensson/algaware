# Generate the description text for every station visit

Builds one prompt per visit and performs them through
[`call_llm_batch()`](https://nodc-sweden.github.io/ifcb-algaware/reference/call_llm_batch.md):
concurrently on providers that support it, sequentially (with
per-station progress) otherwise. A failed request yields the placeholder
text and a warning for that station only, matching the previous
per-station error handling.

## Usage

``` r
generate_station_descriptions(
  visits,
  station_summary,
  taxa_lookup,
  use_llm,
  phyto_groups = NULL,
  llm_provider = NULL,
  on_llm_progress = NULL,
  unclassified_fractions = NULL,
  chl_measure = "fluorescence"
)
```

## Arguments

- visits:

  One row per station visit (from
  [`add_station_sections()`](https://nodc-sweden.github.io/ifcb-algaware/reference/add_station_sections.md)).

- station_summary:

  Aggregated station data.

- taxa_lookup:

  Optional taxa lookup table.

- use_llm:

  Logical; FALSE returns placeholders without any request.

- phyto_groups, llm_provider, on_llm_progress:

  Passed through from
  [`add_station_sections()`](https://nodc-sweden.github.io/ifcb-algaware/reference/add_station_sections.md).

- unclassified_fractions:

  Named list of unclassified percentages.

- chl_measure:

  Chlorophyll measurement terminology key.

## Value

Character vector of descriptions, one per visit row.
