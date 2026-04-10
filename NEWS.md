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
