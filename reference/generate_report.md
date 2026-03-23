# Generate the AlgAware Word report

Creates a Word document with biomass maps, heatmaps, stacked bar charts,
station sections, and image mosaics.

## Usage

``` r
generate_report(
  output_path,
  station_summary,
  baltic_wide,
  westcoast_wide,
  baltic_mosaics = list(),
  westcoast_mosaics = list(),
  taxa_lookup = NULL,
  cruise_info = "",
  classifier_name = NULL,
  use_llm = FALSE,
  annotator = "",
  image_counts = NULL,
  total_bio_images = NULL,
  llm_model = NULL,
  n_station_samples = NULL,
  llm_provider = NULL,
  on_llm_progress = NULL
)
```

## Arguments

- output_path:

  Path for the output .docx file.

- station_summary:

  Aggregated station data.

- baltic_wide:

  Wide-format Baltic summary.

- westcoast_wide:

  Wide-format West Coast summary.

- baltic_mosaics:

  Named list of mosaic images (Baltic).

- westcoast_mosaics:

  Named list of mosaic images (West Coast).

- taxa_lookup:

  Optional taxa lookup table with `HAB` column.

- cruise_info:

  Character string with cruise/date information for title.

- classifier_name:

  Optional character string with the classifier model name used for
  automated classification.

- use_llm:

  Logical; if TRUE and an LLM API key is configured (OPENAI_API_KEY or
  GEMINI_API_KEY), generate report text using an LLM. Default FALSE uses
  placeholder text.

- annotator:

  Character string with the analyst name for the introduction statement.

- image_counts:

  Optional data frame from
  [`fetch_image_counts()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/fetch_image_counts.md)
  with cruise-wide image counts and coordinates for the track map.

- total_bio_images:

  Optional integer; total number of biological images used in the report
  (excluding non-biological classes).

- llm_model:

  Optional character string; name of the LLM model used for text
  generation (e.g. `"gpt-4.1"`).

- n_station_samples:

  Optional integer; total number of IFCB samples matched to AlgAware
  stations.

- llm_provider:

  Optional character string; LLM provider to use (`"openai"` or
  `"gemini"`). NULL auto-detects.

- on_llm_progress:

  Optional callback function called before each LLM request with
  arguments `(step, total, detail)` for progress reporting.

## Value

Invisible path to the created document.
