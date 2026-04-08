# Generate the AlgAware Word report

Creates a Word document with biomass maps, heatmaps, stacked bar charts,
station sections, and image mosaics. Optionally includes a front page
with logo, diary number placeholder, and two summary mosaics.

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
  on_llm_progress = NULL,
  frontpage_baltic_mosaic = NULL,
  frontpage_westcoast_mosaic = NULL,
  unclassified_fractions = NULL,
  frontpage_baltic_taxa = NULL,
  frontpage_westcoast_taxa = NULL,
  report_number = NULL,
  report_dnr = NULL,
  ctd_data = NULL,
  ctd_data_full = NULL,
  lims_data = NULL,
  lims_data_full = NULL,
  chl_stats = NULL,
  chl_map_source = "ferrybox",
  phyto_groups = NULL
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
  [`fetch_image_counts()`](https://nodc-sweden.github.io/ifcb-algaware/reference/fetch_image_counts.md)
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

- frontpage_baltic_mosaic:

  Optional magick image for the front page Baltic Sea mosaic. If NULL
  (together with West Coast), no front page is generated.

- frontpage_westcoast_mosaic:

  Optional magick image for the front page West Coast mosaic.

- unclassified_fractions:

  Optional named list or data frame of unclassified proportion estimates
  passed to the LLM prompts for context.

- frontpage_baltic_taxa:

  Optional character vector of taxa names matching the numbered images
  in the Baltic mosaic (for captions).

- frontpage_westcoast_taxa:

  Optional character vector of taxa names matching the numbered images
  in the West Coast mosaic.

- report_number:

  Optional report issue number (e.g. `"1"`).

- report_dnr:

  Optional diarienummer string for the front page (e.g. `"2026-1234"`).

- ctd_data:

  Optional CTD profile data from
  [`read_cnv_folder()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder.md)
  (AlgAware-matched, used for the chlorophyll map).

- ctd_data_full:

  Optional full CTD profile data from
  [`read_cnv_folder_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder_all.md)
  (all standard stations, used for the CTD section with regional
  fluorescence profiles and time series).

- lims_data:

  Optional LIMS discrete Chl-a data from
  [`read_lims_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data.md)
  (AlgAware-matched, used for the chlorophyll map).

- lims_data_full:

  Optional LIMS data from
  [`read_lims_data_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data_all.md)
  (all standard stations, used for the CTD section time series).

- chl_stats:

  Optional historical statistics from
  [`load_chl_statistics()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_chl_statistics.md).

- chl_map_source:

  Character; active chlorophyll map source: `"ferrybox"`, `"ctd"`,
  `"lims"` (bottle, 0-20 m), or `"lims_hose"` (hose integrated, 0-10 m).

- phyto_groups:

  Optional data frame with columns `name`, `AphiaID`, and `phyto_group`
  (typically from
  [`SHARK4R::assign_phytoplankton_group()`](https://sharksmhi.github.io/SHARK4R/reference/assign_phytoplankton_group.html)).
  Used to render the per-station phytoplankton group composition pie
  map. If `NULL`, the assignments are computed on demand via SHARK4R
  when available.

## Value

Invisible path to the created document.
