# AlgAware-IFCB

An interactive R/Shiny application for processing, validating, and reporting
phytoplankton data from Imaging FlowCytobot (IFCB) instruments. Developed for
Swedish coastal monitoring at SMHI.

AlgAware-IFCB integrates with the IFCB Dashboard for automated data retrieval,
provides an interactive image gallery for reviewing and correcting AI classifier
predictions, and generates Word reports with biomass maps, heatmaps, and image
mosaics.

## Installation

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("anderstorstensson/algaware")
```

## Usage

```r
library(algaware)
launch_app()
```

The application opens in your default browser.

## Features

### Data Loading

- Fetch metadata from any IFCB Dashboard instance
- Filter by cruise number or date range
- Automatic spatial matching of IFCB bins to monitoring stations in the Baltic Area
- Download raw data, features, and HDF5 classification files
- FerryBox chlorophyll integration (optional)

### Image Gallery and Validation

- Browse classified images by taxon and region (Baltic Sea / West Coast)
- Searchable class dropdown for quick navigation across 100+ classes
- Visual tags for HAB species and non-biological classes
- Select individual images or entire pages
- Relabel selected images or entire classes
- Invalidate misclassified taxa
- Store manual annotations to SQLite (compatible with ClassiPyR)
- Measurement tool for on-screen size estimation

### Visualizations

- **Maps**: Biomass and chlorophyll distribution across stations
- **Heatmaps**: Biovolume by taxon and station visit
- **Stacked bar charts**: Relative biovolume composition (top 10 taxa)
- **Summary table**: Interactive, sortable station-level data

### Report Generation

- Automated Word document (.docx) with all plots and station sections
- Image mosaics for top taxa per region (adaptive layout for chains vs.
  compact organisms)
- HAB species annotations throughout
- Classifier model attribution
- Corrections log export (CSV)

## Configuration

Settings are persisted in `~/.config/R/algaware/settings.json` and can be
edited through the in-app Settings panel:

| Setting | Description | Default |
|---------|-------------|---------|
| Dashboard URL | IFCB Dashboard base URL | -- |
| Dashboard Dataset | Dataset name | -- |
| Classification Path | Directory with HDF5 classifier output | -- |
| FerryBox Data Path | Directory with FerryBox CSV files | -- |
| Local Storage Path | Where downloaded data is cached | `./algaware_data` |
| Database Folder | Directory for annotations.sqlite | -- |
| Non-biological Classes | Comma-separated list of classes excluded from analysis | `detritus,Debris,Air_bubbles,Beads,mix` |
| Pixels per Micron | IFCB camera calibration factor | `2.77` |

Extra monitoring stations can be added from the SHARK station register through
the Settings panel.

## Bundled Data

- **Taxa lookup** (`inst/extdata/taxa_lookup.csv`): Phytoplankton taxa with
  WoRMS AphiaID references and HAB status flags
- **Station definitions** (`inst/stations/algaware_stations.tsv`): 12 AlgAware
  monitoring stations (6 Baltic Sea, 6 West Coast)
- **Report template** (`inst/templates/report_template.docx`): Word document
  template for generated reports

## Project Structure

```
algaware/
├── R/
│   ├── run_app.R              # launch_app() entry point
│   ├── mod_settings.R         # Settings module
│   ├── mod_data_loader.R      # Data loading pipeline
│   ├── mod_gallery.R          # Image gallery browser
│   ├── mod_validation.R       # Annotation and relabeling
│   ├── mod_report.R           # Report generation module
│   ├── data_download.R        # Dashboard API integration
│   ├── data_processing.R      # Biovolume and aggregation
│   ├── database.R             # SQLite operations
│   ├── plots.R                # Maps, heatmaps, bar charts
│   ├── mosaics.R              # Adaptive image mosaics
│   ├── stations.R             # Station matching
│   ├── report.R               # Word document builder
│   └── utils.R                # Settings and utilities
├── inst/
│   ├── app/                   # Shiny app (ui.R, server.R)
│   ├── extdata/               # Taxa lookup
│   ├── stations/              # Station definitions
│   └── templates/             # Report template
└── tests/testthat/            # Test suite
```

## Testing

```r
devtools::test()
```

## License

MIT
