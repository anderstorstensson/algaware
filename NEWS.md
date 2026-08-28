# algaware (development version)

## Bug fixes

- The grey background fill of report mosaics now matches the actual image
  backdrop. A channel-indexing bug made the fill color the median of each
  image's top pixel rows instead of the whole-image median, producing too
  bright backgrounds for instruments with a bright top-edge artifact (e.g.
  the west coast mosaics). A fractional median could also silently replace
  the fill with the bright `#F0F0F0` fallback; medians are now rounded.

# algaware 0.3.1

## New features

- Storing annotations now backfills the remaining ROIs of each affected
  sample as `"unclassified"` with `is_manual = 0` (not yet reviewed), so
  every saved sample is fully represented in the shared annotations
  database -- the same convention as ClassiPyR's `fill_unclassified_db()`,
  which downstream analysis relies on. The backfill never overwrites
  existing annotations, so saving some images of a sample to one class and
  other images to another class later works as before. Explicitly storing
  reviewed images as `"unclassified"` is now also accepted (saved with
  `is_manual = 1`).

# algaware 0.3.0

## New features

- Corrections are now auto-saved as crash protection: whenever you navigate
  to another class or region (and on closing the app), the corrections CSV
  -- identical to the "Download corrections" export -- is written to
  `<Local Storage Path>/corrections/algaware_corrections_<date>.csv` if
  anything changed since the last save. After a crash (or after forgetting
  to download), reload the cruise data and restore the file with the
  existing "Import corrections" button. The first save of a new session
  sets any autosave left by an earlier session aside as
  `algaware_corrections_<date>_prev.csv`, so starting to validate after a
  restart cannot overwrite the file you are trying to recover.
- Corrections exports now include the `custom_is_diatom` column, so a
  re-imported custom class keeps its "Is diatom" flag (it previously came
  back as not-a-diatom). Older corrections files without the column still
  import fine, with `is_diatom` defaulting to `FALSE` as before.
- After loading data, the app now warns about near-empty bins (fewer than
  20 images), which are typically end-of-cruise IFCB cleaning-cycle samples
  of distilled water that happen to coincide with an AlgAware station. The
  warning lists the affected bins with station names and image counts,
  points to the Samples tab for exclusion, and stays on screen until closed
  so it is not missed while working elsewhere in the app.
- The report heatmaps now show which phytoplankton group each row belongs
  to: rows are split into one panel per group (Diatoms, Dinoflagellates,
  Cyanobacteria, Cryptophytes, Ciliates, Silicoflagellates, Other) with a
  coloured group label on the left, using the same colours as the pie-chart
  group map. Ciliates are grouped together in the heatmap -- including
  *Mesodinium*, which keeps its own slice in the pie charts -- via a new
  `Ciliates` group (phylum Ciliophora) in `inst/config/phyto_groups.yaml`.
  Note that SHARK4R applies the custom groups in YAML order with later rules
  overwriting earlier matches, so broad rules must come before specific ones.
  To make room for the group labels, the biovolume colour bar now sits
  horizontally above the heatmap, and with more than eight station visits
  the x-axis labels switch to a compact vertical two-line form
  (`STATION (n = X)` over the date) so they no longer overlap.

## Minor improvements and fixes

- Loading a new cruise now clears the corrections log, custom classes and
  gallery selection from the previous cruise. Before, corrections made on
  one cruise were carried into the next cruise loaded in the same session and
  ended up mixed into its exported and auto-saved corrections file.
- Station descriptions no longer fall back to the "(LLM generation failed)"
  placeholder when every image at a visit is unclassified (e.g. a near-empty
  cleaning-cycle bin); the prompt now states that no classified taxa were
  recorded.
- Heatmap row labels are HTML-escaped, so a custom class name containing
  `<`, `>` or `&` no longer aborts report generation.

- The "unclassified" classifier category is no longer passed to the LLM
  prompts, so generated station descriptions and cruise summaries no longer
  describe it as a dominant taxon. Totals, percentages and group breakdowns
  in the text are computed from classified taxa only; figures and tables
  still include the class.
- CTD fluorescence profiles now use a fixed 0-10 µg/L chlorophyll scale
  shared by all basins so profiles are directly comparable. Only when any
  value (CTD fluorescence or same-cruise bottle Chl-a, 0-50 m) exceeds
  10 µg/L does the scale grow dynamically, and that scale is then inherited
  by every region figure.
- Heatmap taxa are now ordered by phytoplankton group (Diatoms,
  Dinoflagellates, Cyanobacteria, etc.) and alphabetically within each
  group, instead of by total biovolume, in both the app and the report.
  Biovolume contribution is still shown in the stacked bar charts. When
  group assignments are unavailable the previous biovolume ordering is used.
- Fix the Samples table opening sorted by ROI file size instead of sample
  time (off-by-one column index), which scrambled the cruise-track order.
- The red-asterisk HAB legend in the report introduction is now printed only
  when a HAB-flagged taxon actually appears in the report.
- Selecting a cruise against dashboard metadata that has no cruise column
  now raises a clear error instead of silently returning (and downloading)
  the entire unfiltered dataset; rows with missing cruise or timestamp are
  dropped instead of appearing as phantom all-NA rows.
- Error messages in the data-loading sidebar no longer lose their context:
  only R's "Error in <call>: " prefix is stripped, instead of everything up
  to the last colon (which reduced e.g. a download error to "'404 Not
  Found'" without the URL).
- Fix dead class-navigation arrows after "Unclassify Selected" or
  "Unclassify Class" emptied the region's last class: the class index is
  now clamped to the shrunken class list, as "Relabel Class" already did.
- Fix extra stations added in Settings permanently breaking the app on the
  next restart: the settings loader turned the saved station list into a
  data frame, crashing data loading and the Settings tab with "$ operator is
  invalid for atomic vectors". Settings files mangled by the old code are
  repaired automatically on load.
- Fix stale gallery selections corrupting stored annotations: selections now
  clear when navigating to another class or region, and "Store Annotations"
  saves each image under its actual class instead of stamping the currently
  displayed class onto the whole selection (which could silently overwrite
  correct annotations in the database).
- Fix a crash (R session segfault) when building a mosaic from only one or
  two images, e.g. a front-page mosaic for a region with few taxa.
- Fix the report post-processing silently corrupting every non-ASCII
  character (µ, Å/Ä/Ö, –) in the generated Word document on hosts running a
  non-UTF-8 locale (e.g. servers under a C/POSIX locale). The OOXML parts
  are now read and written as raw bytes instead of through locale-dependent
  text connections.
- The "Relabel Selected" and "Relabel Class" dialogs no longer open with the
  first class preselected; confirming without picking a target previously
  relabelled images to an arbitrary class with no undo.
- Fix the Maps tab and report generation crashing with "undefined columns
  selected" when the WoRMS lookup is unavailable (offline use): the
  offline fallback produced a differently shaped phytoplankton-group table
  than the online path. `assign_phyto_groups()` now returns a plain vector
  of group names aligned with its input.
- Fix a failed cruise load leaving the app half-loaded (the new cruise's
  station metadata combined with the previous cruise's classifications),
  which could later crash summary recomputation. Loaded state is now
  committed only after processing succeeds.
- Define and export the `%||%` operator so the app works on R 4.1-4.3, where
  base R does not provide it; previously the Maps tab and metadata fetching
  failed there with `could not find function "%||%"`.
- Fix a failed summary recomputation (e.g. a missing feature file after
  excluding a sample) permanently leaving the summary tables, heatmaps and
  maps describing the previous sample set. The failure is now reported as a
  notification and recomputation retries on the next tab switch.
- Fix a failed report generation leaving the previous report downloadable
  and labelled "Report ready for download"; the download state is now
  invalidated when a new generation starts.
- Fix the biomass and phytoplankton-group maps silently discarding whole
  rows when a single value was missing: a station visit with a missing
  sample volume under-reported carbon biomass and skewed pie-chart group
  proportions, and a station missing from the SHARK register vanished
  without warning. Missing values are now tolerated per column and stations
  without coordinates are reported.
- Fix CTD casts without a parsable timestamp being silently discarded during
  cast deduplication, which removed the whole station from the CTD tab and
  the report.
- Fix double-counted biomass for stations listed twice in the SHARK station
  register with different coordinates (e.g. "G2"): the coordinate join now
  keeps a single row per station name.
- Fix report text formatting for HAB-flagged genera: an asterisk was
  inserted in the middle of full binomials ("Dinophysis* acuminata"), and
  custom classes with missing HAB/italic flags caused the literal word "NA"
  to be asterisked or italicised throughout the report.
- Fix an LLM response without text content (e.g. a refusal or safety block)
  aborting the whole report generation after all API calls completed; such
  responses now fall back to the manual placeholder text like other LLM
  errors.
- Fix the station description silently falling back to placeholder text when
  a warning-level taxon had a missing cell count (missing sample volume).
- Gallery robustness fixes: an empty image folder no longer replaces the
  gallery with an error box (the "No images to display" fallback shows
  instead); the page indicator resets when the page size changes or a new
  dataset is loaded; images that fail to load now show their "Not found"
  placeholder; and a drag-select released over empty space no longer
  swallows the next image click.
- Fix importing a corrections log re-including samples that had been
  excluded in the Samples tab, which inflated the report's image totals
  until an exclusion was toggled again.
- Fix a failed CTD reload keeping the previous cruise's CTD/LIMS data loaded
  and reportable; the CTD state is now cleared when a new load starts.
- Fix the regional CTD figure losing all x-axis labels when the last station
  in the region produced no panel (e.g. all its casts were deduplicated
  away).
- Fix closing one app session blanking the front-page mosaic thumbnails in
  all other open sessions of the same R process; the image resource path is
  now per session.
- Fix a single sample without a valid analysed volume switching the whole
  image concentration map from counts per litre to raw image counts; only
  the affected samples are now dropped (with a warning).
- Generated reports no longer accumulate in the server's temp directory, and
  two sessions generating a report in the same second can no longer
  overwrite each other's file.
- Fix duplicated station biomass in the report when using the CTD
  chlorophyll source: repeat casts with slightly different coordinates
  produced multiple chlorophyll rows per station, and the merge duplicated
  every taxon row of that station.
- Fix duplicated "Image mosaics" headings and repeated "Mosaic 1." captions
  when both regions had mosaics: the report now has a single section heading
  and continuous mosaic numbering.
- Fix the unclassified-image percentage sometimes being attached to the
  wrong station visit in the report: visit numbering is now derived once
  from the sample metadata instead of separately from two different row
  sets that could disagree when a station was visited twice.
- Fix a sample with a missing timestamp crashing the whole station
  aggregation; it is now grouped with the current visit.
- Fix feature files being re-downloaded on every reload: the cached-file
  check compared the "_features.csv" file names against bare sample IDs and
  never matched.
- Fix interrupted raw-data downloads never being retried: a sample now
  counts as downloaded only when all three files (.roi, .adc, .hdr) are
  present, and a failed download is reported in the progress status instead
  of "Raw data downloaded". Missing .hdr files previously made ml_analyzed
  unavailable, silently inflating per-litre concentrations.
- Move `yaml` from Suggests to Imports: it is required unconditionally for
  phytoplankton-group assignment, and installations without it silently
  produced reports with no group classification. A failed group assignment
  during report generation now emits a warning instead of being swallowed.
- Fix LLM prompt assembly for stations with missing sample volumes: an
  all-NA station no longer loses its description to a "no rows to
  aggregate" error, and missing counts/biovolume values are now spelled out
  as "not available" in the prompt instead of a literal "NA", which gave the
  model no signal that the value was missing.
- Fix a read-only or locked annotations database aborting the whole cruise
  load: read paths no longer run schema DDL and degrade gracefully (empty
  annotations / auto-generated class list, with a warning). All database
  connections now set a 5-second busy timeout so brief write locks from
  ClassiPyR wait instead of failing immediately.
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
