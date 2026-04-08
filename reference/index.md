# Package index

## Launch Application

Start the AlgAware Shiny app

- [`launch_app()`](https://nodc-sweden.github.io/ifcb-algaware/reference/launch_app.md)
  : Launch the AlgAware Shiny Application

## Data Download

Fetch data from IFCB Dashboard

- [`fetch_dashboard_metadata()`](https://nodc-sweden.github.io/ifcb-algaware/reference/fetch_dashboard_metadata.md)
  : Fetch metadata from the IFCB Dashboard
- [`fetch_image_counts()`](https://nodc-sweden.github.io/ifcb-algaware/reference/fetch_image_counts.md)
  : Fetch image count metadata from the IFCB Dashboard
- [`filter_metadata()`](https://nodc-sweden.github.io/ifcb-algaware/reference/filter_metadata.md)
  : Filter metadata by cruise number or date range
- [`download_raw_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/download_raw_data.md)
  : Download raw IFCB files for selected bins
- [`download_features()`](https://nodc-sweden.github.io/ifcb-algaware/reference/download_features.md)
  : Download feature files for selected bins

## Data Processing

Process and aggregate IFCB data

- [`aggregate_station_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/aggregate_station_data.md)
  : Aggregate biovolume data per station visit
- [`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md)
  : Summarize biovolumes for classified bins
- [`create_wide_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_wide_summary.md)
  : Create wide-format summary for a region
- [`parse_non_bio_classes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/parse_non_bio_classes.md)
  : Parse non-biological classes from comma-separated string
- [`compute_unclassified_fractions()`](https://nodc-sweden.github.io/ifcb-algaware/reference/compute_unclassified_fractions.md)
  : Compute the percentage of unclassified images per station visit
- [`match_bins_to_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/match_bins_to_stations.md)
  : Match dashboard metadata to AlgAware stations using spatial join
- [`collect_ferrybox_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/collect_ferrybox_data.md)
  : Collect ferrybox data for station visits

## Classification

Read and resolve classifier data

- [`read_h5_classifications()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_h5_classifications.md)
  : Read classifications from H5 files
- [`read_classifier_name()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_classifier_name.md)
  : Read classifier name from an H5 classification file
- [`resolve_class_list()`](https://nodc-sweden.github.io/ifcb-algaware/reference/resolve_class_list.md)
  : Resolve the active class list
- [`copy_classification_files()`](https://nodc-sweden.github.io/ifcb-algaware/reference/copy_classification_files.md)
  : Copy classification H5 files for selected bins

## CTD & Chlorophyll Data

Parse CTD profiles and LIMS chlorophyll data for the chlorophyll map

- [`load_chl_statistics()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_chl_statistics.md)
  : Load bundled historical Chl-a statistics (1991-2020)
- [`load_station_mapper()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_station_mapper.md)
  : Load station name synonym mapper
- [`load_standard_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_standard_stations.md)
  : Load standard monitoring stations with regional assignments
- [`read_cnv_folder()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder.md)
  : Parse all CNV files from a folder tree (AlgAware-matched, for chl
  map)
- [`read_cnv_folder_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder_all.md)
  : Parse all CNV files from a folder tree (all standard stations)
- [`read_lims_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data.md)
  : Read LIMS discrete chlorophyll data (AlgAware-matched)
- [`read_lims_data_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data_all.md)
  : Read LIMS discrete chlorophyll data (all standard stations)
- [`compute_ctd_chl_avg()`](https://nodc-sweden.github.io/ifcb-algaware/reference/compute_ctd_chl_avg.md)
  : Compute 0-20m depth-averaged chlorophyll from CTD data
- [`compute_lims_chl_avg()`](https://nodc-sweden.github.io/ifcb-algaware/reference/compute_lims_chl_avg.md)
  : Compute 0-20m depth-averaged chlorophyll from LIMS bottle data
- [`compute_lims_hose_avg()`](https://nodc-sweden.github.io/ifcb-algaware/reference/compute_lims_hose_avg.md)
  : Compute chlorophyll from LIMS hose (integrated 0-10 m) samples

## Visualization

Create plots and maps

- [`create_pie_map()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_pie_map.md)
  : Pie chart map with displacement and leader lines
- [`create_group_map()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_group_map.md)
  : Create a phytoplankton group composition map
- [`create_biomass_maps()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_biomass_maps.md)
  : Create biomass and chlorophyll maps
- [`create_chl_map()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_chl_map.md)
  : Create a standalone chlorophyll map
- [`create_ctd_region_figure()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_ctd_region_figure.md)
  : Create a regional CTD composite figure (all standard stations)
- [`create_heatmap()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_heatmap.md)
  : Create a heatmap of biovolume by species and station
- [`create_stacked_bar()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_stacked_bar.md)
  : Create a stacked bar chart of relative biovolume
- [`create_image_count_map()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_image_count_map.md)
  : Create an image concentration map from cruise metadata

## Reports

Generate Word reports

- [`generate_report()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_report.md)
  : Generate the AlgAware Word report
- [`generate_frontpage_mosaic()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_frontpage_mosaic.md)
  : Generate a numbered frontpage mosaic
- [`generate_english_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_english_summary.md)
  : Generate the English summary text
- [`generate_swedish_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_swedish_summary.md)
  : Generate the Swedish summary text
- [`generate_station_description()`](https://nodc-sweden.github.io/ifcb-algaware/reference/generate_station_description.md)
  : Generate a station description

## Database

SQLite database operations

- [`load_annotations_db()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_annotations_db.md)
  : Load annotations from SQLite
- [`save_annotations_db()`](https://nodc-sweden.github.io/ifcb-algaware/reference/save_annotations_db.md)
  : Save selected annotations to SQLite
- [`load_global_class_list_db()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_global_class_list_db.md)
  : Load global class list from SQLite
- [`save_global_class_list_db()`](https://nodc-sweden.github.io/ifcb-algaware/reference/save_global_class_list_db.md)
  : Save global class list to SQLite
- [`load_algaware_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_algaware_stations.md)
  : Load AlgAware station definitions
- [`load_taxa_lookup()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_taxa_lookup.md)
  : Load the bundled taxa lookup table
- [`get_db_path()`](https://nodc-sweden.github.io/ifcb-algaware/reference/get_db_path.md)
  : Get path to the annotations SQLite database

## Mosaics

Image mosaic generation

- [`create_mosaic()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_mosaic.md)
  : Create an adaptive image mosaic for a taxon
- [`create_region_mosaics()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_region_mosaics.md)
  : Create mosaics for top taxa in a region
- [`get_top_taxa()`](https://nodc-sweden.github.io/ifcb-algaware/reference/get_top_taxa.md)
  : Get top taxa by total biovolume from a wide summary
- [`get_taxon_rois()`](https://nodc-sweden.github.io/ifcb-algaware/reference/get_taxon_rois.md)
  : Get ROI information for a specific taxon in a set of samples
- [`extract_random_taxon_image()`](https://nodc-sweden.github.io/ifcb-algaware/reference/extract_random_taxon_image.md)
  : Extract a single random image for a taxon

## LLM Support

AI-powered report text generation

- [`llm_available()`](https://nodc-sweden.github.io/ifcb-algaware/reference/llm_available.md)
  : Check if LLM text generation is available
- [`llm_provider()`](https://nodc-sweden.github.io/ifcb-algaware/reference/llm_provider.md)
  : Detect the default LLM provider
- [`llm_providers()`](https://nodc-sweden.github.io/ifcb-algaware/reference/llm_providers.md)
  : List available LLM providers
- [`llm_model_name()`](https://nodc-sweden.github.io/ifcb-algaware/reference/llm_model_name.md)
  : Get the model name for a provider

## Shiny Modules

UI and server module functions

- [`mod_ctd_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_ctd_server.md)
  : CTD Module Server
- [`mod_ctd_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_ctd_ui.md)
  : CTD Module UI
- [`mod_data_loader_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_data_loader_server.md)
  : Data Loader Module Server
- [`mod_data_loader_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_data_loader_ui.md)
  : Data Loader Module UI
- [`mod_frontpage_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_frontpage_server.md)
  : Front Page Mosaic Module Server
- [`mod_frontpage_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_frontpage_ui.md)
  : Front Page Mosaic Module UI
- [`mod_gallery_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_gallery_server.md)
  : Gallery Module Server
- [`mod_gallery_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_gallery_ui.md)
  : Gallery Module UI
- [`mod_report_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_report_server.md)
  : Report Module Server
- [`mod_report_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_report_ui.md)
  : Report Module UI
- [`mod_samples_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_samples_server.md)
  : Sample Exclusion Module Server
- [`mod_samples_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_samples_ui.md)
  : Sample Exclusion Module UI
- [`mod_settings_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_settings_server.md)
  : Settings Module Server
- [`mod_settings_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_settings_ui.md)
  : Settings Module UI
- [`mod_validation_server()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_validation_server.md)
  : Validation Module Server
- [`mod_validation_ui()`](https://nodc-sweden.github.io/ifcb-algaware/reference/mod_validation_ui.md)
  : Validation Module UI

## Utilities

Settings and helper functions

- [`load_settings()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_settings.md)
  : Load persistent settings
- [`save_settings()`](https://nodc-sweden.github.io/ifcb-algaware/reference/save_settings.md)
  : Save settings to disk
- [`merge_custom_taxa()`](https://nodc-sweden.github.io/ifcb-algaware/reference/merge_custom_taxa.md)
  : Merge Custom Classes into Taxa Lookup
