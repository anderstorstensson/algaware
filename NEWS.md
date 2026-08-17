# algaware (development version)

- Fix a crash ("could not find function `build_cruise_info`") when excluding
  a sample in the installed app: the helper is now exported so
  `inst/app/server.R` can call it.
- Fix gallery selection state getting out of sync between the browser and the
  server. Previously the highlights could show images as deselected while
  they were still selected server-side (or vice versa), so a later relabel or
  store-annotations action could silently include images from an earlier
  selection. The server-side selection is now mirrored to the browser on
  every change and re-applied after each gallery re-render.

# algaware 0.2.0

## Minor improvements and fixes

- Describe chlorophyll in the generated report text according to the selected
  chlorophyll source: FerryBox and CTD values are "chlorophyll fluorescence",
  while LIMS bottle and hose (0-10 m integrated) values are "chlorophyll-a
  concentration" (measured on a filter). Previously the text always referred
  to fluorescence regardless of the source.
- Name the downloaded Word report `Algaware_<Report No>.docx` using the
  "Report No" entered in the Report tab, falling back to `Algaware_X.docx`
  when no number is entered.
- Abbreviate repeated species binomials across the whole Station reports
  section: a species written out in full at one station (e.g.
  *Nodularia spumigena*) is abbreviated (*N. spumigena*) at any later station,
  following standard biological convention.
- Also abbreviate repeated binomials within each summary (English and Swedish
  treated separately): a species spelled out in full in the West Coast part or
  the HAB sentence is abbreviated where it recurs in the Baltic part of the
  same summary.
- Order report sections consistently with West Coast before Baltic Sea
  throughout (heatmaps, relative-biovolume bars, station reports, image
  mosaics and the front-page mosaic overview), matching the order already
  used in the summary/abstract.
- Fix the generated Word report sometimes disappearing (failed download) when
  page numbering was post-processed. The report is no longer deleted before
  being rebuilt, and the rebuild uses the `zip` package instead of an external
  `zip` executable that may be missing on some servers.
- Stop Microsoft Word prompting to update fields ("This document contains
  fields that may refer to other files...") when opening the report, by
  clearing the dirty-field flags on the page-number field. Page numbers still
  update automatically.
- Fix station-visit aggregation silently dropping taxa whose biovolume or
  carbon value was missing (e.g. a failed or missing feature file). Such rows
  were discarded entirely, including their valid cell counts, which
  under-reported counts and skewed the per-litre concentrations and presence
  categories. Aggregation now tolerates `NA` measures per column.
- Fix a double-counting risk when re-joining `AphiaID` after aggregation: a
  taxon name mapping to more than one `AphiaID` could duplicate its rows and
  inflate biovolume in the report. The join now keeps a single `AphiaID` per
  taxon, preferring a non-missing value.
- Require `ggplot2` (>= 3.4.0) for the `linewidth` aesthetic used in CTD
  figures, and `tidyr` (>= 1.1.0) for `pivot_wider()`/`pivot_longer()`.
- Split llm.R, plots.R and report.R into manageable file sizes
- Migrate pie chart plotting from internal functions to `SHARK4R` 1.2.0
- Fix biomass and chlorophyll maps failing with "no rows to aggregate" when
  FerryBox provides no valid chlorophyll readings. The chlorophyll column is
  now omitted when entirely missing, and map aggregation tolerates all-`NA`
  chlorophyll.
- Allow downloading the corrections log at any time after making corrections,
  without first generating the Word report.
- Fix the validation gallery getting stuck in an infinite loop, switching back
  and forth between two classes, when the class navigation arrows were clicked
  while a whole-class relabel was still in progress. Selectize echo events can
  no longer feed back into the current class index.
- Add an "Unclassify Selected" button to the Validate tab: a one-click shortcut
  that moves the selected images to "unclassified" without picking a target in
  the Relabel Selected dropdown.
- Rename the "Invalidate Selected" and "Invalidate Class" buttons to "Unclassify
  Selected" and "Unclassify Class", and use the same wording in the validation
  status summary and import preview, so the label matches the resulting
  "unclassified" state.
- Fix stations with Swedish characters (e.g. `Å17`, `SLÄGGÖ`,
  `BY39 ÖLANDS SÖDRA UDDE`) silently dropping out on non-UTF-8 locales (such as
  Windows Server), which also removed whole sampling days from downloads, pie
  charts and the image-count cruise track. Every bundled/external text file is
  now read with its declared encoding (UTF-8 or latin1) and normalised to
  UTF-8, and station-name matching compares on UTF-8, so results no longer
  depend on the host machine's locale.

# algaware 0.1.0

First release.

## Shiny application

- Interactive Shiny app (`launch_app()`) for end-to-end IFCB cruise reporting.
- Sidebar workflow: Settings → Data → Validate → Report.
- Loading overlay with smooth fade-in on first render.

## Data loading

- Downloads IFCB samples from an IFCB Dashboard instance for a selected cruise
  or date range.
- Automatic spatial matching of IFCB samples to AlgAware monitoring stations
  using a configurable bin radius.
- Merges FerryBox chlorophyll fluorescence from a locally configured data
  folder.
- Processes HDF5 classification files together with feature files into 
  biovolumes and carbon estimates using iRfcb.
- SQLite annotation database (ClassiPyR-compatible schema) for exporting
  validated image annotations as training data.

## Validation

- Gallery tab for browsing IFCB images organised by class.
- Per-image and per-class relabelling (reassign to any class in the global
  class list or a custom class) and invalidation.
- Custom class creation with full scientific name, AphiaID, HAB flag, and
  italic formatting metadata.
- Corrections log importable in future sessions to replay relabellings
  automatically.

## Samples tab

- Table view of all samples with exclusion/re-inclusion controls.
- Excluded samples are removed from all summaries, maps, mosaics, and the
  report.

## Images (mosaic designer)

- Interactive mosaic builder for Baltic Sea and West Coast images.
- Binary-search layout algorithm to maximise image size within canvas bounds
  (rectpacker).
- Per-taxon image re-rolling and configurable image count per mosaic.

## Maps and plots

- Station maps: image count, phytoplankton group composition pie chart
  (Diatoms, Dinoflagellates, Cyanobacteria, Cryptophytes, Mesodinium spp.,
  Silicoflagellates, Other), and chlorophyll maps.
- Cyanobacteria pie slice colour: teal-cyan (`#14B8A6`).
- Image count map legend: "Images (counts/L)"; width matched to chlorophyll
  map so y-axes align in the Word report.
- Chlorophyll source selector: FerryBox, CTD fluorescence, LIMS bottle
  (0–20 m), or LIMS hose-integrated (0–10 m).
- Regional heatmaps and stacked bar charts for Baltic Sea and West Coast.
- Summary DT table with per-station taxa and biovolume data.

## CTD panel

- Fluorescence depth profiles per station from CNV files (`oce`).
- Regional Chl-a time-series with 1991–2020 climatological mean ± SD ribbon.
- Automatic station name synonym resolution via `station_mapper.txt`.

## Word report generation

- Generates a Word `.docx` report from an `officer` template with:
  - Front page with logo, diarienummer, phytoplankton group composition pie map, and report number.
  - Swedish summary (Sammanfattning) and English summary (Abstract).
  - Cruise metadata table.
  - Image mosaic overview with numbered captions and italic species formatting.
  - Image count map, chlorophyll map, heatmaps, and stacked bar charts.
  - Per-station sections with image mosaics.
  - CTD fluorescence profiles and Chl-a time-series.
- Species names are italicised in Word output; HAB taxa are marked with a red
  bold asterisk.

## LLM text generation

- Optional AI-generated report text via OpenAI (`OPENAI_API_KEY`) or Google
  Gemini (`GEMINI_API_KEY`).
- Generates Swedish summary, English summary, and individual station
  descriptions guided by a configurable writing guide
  (`inst/extdata/report_writing_guide.md`).
- HAB taxa are deterministically marked with `*` regardless of LLM output.
- Default OpenAI model: `gpt-5.1`. Override via `OPENAI_MODEL` environment
  variable.
- Retry on HTTP 429 (rate-limited) and 503 (service overload) with exponential
  back-off up to 120 s.
- Bloom alert instructions appended to prompts when conditions are met: spring
  bloom (West Coast, Jan–Feb, diatoms > 50 % biovolume at any station,
  chl > 3 µg/L) and cyanobacterial bloom (Baltic, Jun–Aug, cyanobacteria
  > 50 % biovolume at any station, chl > 3 µg/L).

## Taxa lookup and warning levels

- Bundled `taxa_lookup.csv` maps classifier class names to WoRMS scientific
  names, AphiaID, HAB flag, italic formatting, warning level abundance, and 
  sflag qualifiers.
- Custom classes can be added interactively in the app and are merged into
  the taxa lookup at report time.

## Configuration

- `inst/extdata/standard_stations.yaml` — standard CTD stations and regional
  groupings.
- `inst/extdata/station_mapper.txt` — raw station name synonyms for CNV/LIMS
  matching.
- `inst/extdata/annual_1991-2020_statistics_chl20m.txt` — Chl-a climatology
  for CTD time-series ribbon.
- `inst/extdata/report_writing_guide.md` — LLM system prompt / style guide.
- `inst/config/phyto_groups.yaml` — phytoplankton group definitions (class,
  phylum, genus mappings for WoRMS lookup). Edit to add or rename groups
  without touching R code. Read via the exported `assign_phyto_groups()`.
