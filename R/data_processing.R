#' Summarize biovolumes for classified bins
#'
#' Wraps \code{iRfcb::ifcb_summarize_biovolumes()} and joins with taxonomy.
#'
#' @param feature_folder Path to the feature CSV directory.
#' @param hdr_folder Path to the raw data directory (for .hdr files).
#' @param classifications A data.frame from \code{read_h5_classifications()}.
#' @param taxa_lookup A data.frame with columns \code{clean_names},
#'   \code{name}, \code{AphiaID}.
#' @param non_bio_classes Character vector of non-biological class names to
#'   exclude.
#' @param pixels_per_micron Conversion factor from pixels to microns.
#'   Default 2.77, which is the optical calibration constant for the standard
#'   IFCB instrument. Different IFCB units may use slightly different values.
#' @param custom_classes Optional data frame of custom classes with an
#'   \code{is_diatom} column. Used to extend diatom identification.
#' @return A data.frame with per-sample, per-class biovolume data joined with
#'   taxonomy.
#' @export
summarize_biovolumes <- function(feature_folder, hdr_folder, classifications,
                                 taxa_lookup, non_bio_classes = character(0),
                                 pixels_per_micron = 2.77,
                                 custom_classes = NULL) {
  # Build image name (sample_NNNNN format)
  image_names <- paste0(classifications$sample_name, "_",
                        sprintf("%05d", classifications$roi_number))

  # Identify diatom classes from taxa lookup and custom classes
  diatom_classes <- identify_diatom_classes(taxa_lookup, custom_classes)

  biovolume_data <- iRfcb::ifcb_summarize_biovolumes(
    feature_folder = feature_folder,
    hdr_folder = hdr_folder,
    custom_images = image_names,
    custom_classes = classifications$class_name,
    diatom_include = diatom_classes,
    micron_factor = 1 / pixels_per_micron,
    verbose = FALSE
  )

  # Join with taxonomy
  biovolume_data <- merge(
    biovolume_data,
    taxa_lookup[, c("clean_names", "name", "AphiaID")],
    by.x = "class",
    by.y = "clean_names",
    all.x = TRUE
  )

  # Remove non-biological classes
  if (length(non_bio_classes) > 0) {
    biovolume_data <- biovolume_data[!biovolume_data$class %in% non_bio_classes, ]
  }

  biovolume_data
}

#' Identify diatom classes from taxa lookup
#'
#' Diatoms require a different biovolume formula than other phytoplankton
#' (they have silica frustules that affect the carbon:biovolume ratio).
#' This function matches class names against known diatom genera so that
#' \code{iRfcb::ifcb_summarize_biovolumes()} can apply the correct formula.
#'
#' @param taxa_lookup A data.frame with \code{clean_names} column.
#' @return Character vector of class names likely to be diatoms.
#' @keywords internal
identify_diatom_classes <- function(taxa_lookup, custom_classes = NULL) {
  # Genus-level patterns for diatom taxa (Bacillariophyta)
  diatom_patterns <- c(
    "Navicula", "Actinocyclus", "Achnanthes", "Proboscia", "rhizosolenia",
    "Chaetocero", "centrales", "Centrales", "Coscinodiscus", "Thalassiosira",
    "Skeletonema", "Pseudo-nitzschia", "Nitzschia", "Ditylum", "Guinardia",
    "Dactyliosolen", "Lauderia", "Leptocylindrus", "Eucampia", "Corethron",
    "Melosira", "Paralia", "Fragilaria", "Asterionella", "Cerataulina",
    "Pauliella", "Pennales", "Odontella", "Porosira", "Stellarima",
    "Sundstroemia", "Thalassionema", "Trieres", "Diatoma", "Licmophora",
    "Striatella", "Cocconeis", "Cylindrotheca"
  )

  pattern <- paste(diatom_patterns, collapse = "|")
  diatoms <- taxa_lookup$clean_names[grepl(pattern, taxa_lookup$clean_names)]

  # Include custom classes flagged as diatoms
  if (!is.null(custom_classes) && nrow(custom_classes) > 0 &&
      "is_diatom" %in% names(custom_classes)) {
    custom_diatoms <- custom_classes$clean_names[custom_classes$is_diatom]
    diatoms <- unique(c(diatoms, custom_diatoms))
  }

  diatoms
}

#' Compute representative visit dates
#'
#' For each station visit, determines the most common sample date.
#'
#' @param all_data Data.frame with visit_id, STATION_NAME, sample_date columns.
#' @return Data.frame with visit_id, STATION_NAME, visit_date columns.
#' @keywords internal
compute_visit_dates <- function(all_data) {
  visit_dates <- stats::aggregate(
    sample_date ~ visit_id + STATION_NAME,
    data = all_data,
    FUN = function(x) {
      tab <- table(x)
      as.Date(names(tab)[which.max(tab)])
    }
  )
  names(visit_dates)[3] <- "visit_date"
  visit_dates
}

#' Compute sample volumes per station visit
#'
#' @param all_data Data.frame with sample, visit_id, STATION_NAME,
#'   ml_analyzed, sample_time columns.
#' @return Data.frame with visit_id, STATION_NAME, total_ml_analyzed,
#'   median_time columns.
#' @keywords internal
compute_sample_volumes <- function(all_data) {
  sample_meta <- unique(all_data[, c("sample", "visit_id", "STATION_NAME",
                                     "ml_analyzed", "sample_time")])
  sample_volume <- stats::aggregate(
    cbind(total_ml_analyzed = ml_analyzed) ~ visit_id + STATION_NAME,
    data = sample_meta,
    FUN = sum,
    na.rm = TRUE
  )
  n_samples <- stats::aggregate(
    sample ~ visit_id + STATION_NAME,
    data = sample_meta,
    FUN = length
  )
  names(n_samples)[3] <- "n_samples"
  median_time <- stats::aggregate(
    sample_time ~ visit_id + STATION_NAME,
    data = sample_meta,
    FUN = stats::median
  )
  names(median_time)[3] <- "median_time"
  result <- merge(sample_volume, n_samples, by = c("visit_id", "STATION_NAME"))
  merge(result, median_time, by = c("visit_id", "STATION_NAME"))
}

#' Compute per-liter concentrations from aggregated data
#'
#' @param agg Data.frame with total_counts, total_biovolume_mm3,
#'   total_carbon_ug, total_ml_analyzed columns.
#' @return The input data.frame with added counts_per_liter,
#'   biovolume_mm3_per_liter, carbon_ug_per_liter columns.
#' @keywords internal
compute_per_liter <- function(agg) {
  ml_liters <- agg$total_ml_analyzed / 1000
  ml_liters[ml_liters <= 0 | is.na(ml_liters)] <- NA_real_
  agg$counts_per_liter <- agg$total_counts / ml_liters
  agg$biovolume_mm3_per_liter <- agg$total_biovolume_mm3 / ml_liters
  agg$carbon_ug_per_liter <- agg$total_carbon_ug / ml_liters
  agg
}

#' Compute presence categories based on percentage of total counts
#'
#' Categories follow the SHARK/HELCOM abundance scale:
#' 5 = dominant (>=50%), 4 = abundant (>=10%),
#' 3 = common (>=1%), 2 = scarce (>=0.1%), 1 = rare (>0%), 0 = absent.
#'
#' @param agg Data.frame with visit_id, STATION_NAME, counts_per_liter.
#' @return The input data.frame with added pct and Presence_cat columns.
#' @keywords internal
compute_presence_categories <- function(agg) {
  station_totals <- stats::aggregate(
    counts_per_liter ~ visit_id + STATION_NAME,
    data = agg,
    FUN = sum,
    na.rm = TRUE
  )
  names(station_totals)[3] <- "sample_counts"
  agg <- merge(agg, station_totals, by = c("visit_id", "STATION_NAME"),
               all.x = TRUE)

  agg$pct <- ifelse(agg$sample_counts > 0,
                    agg$counts_per_liter / agg$sample_counts, 0)

  agg$Presence_cat <- ifelse(
    agg$pct >= 0.50, 5L,
    ifelse(agg$pct >= 0.10, 4L,
    ifelse(agg$pct >= 0.01, 3L,
    ifelse(agg$pct >= 0.001, 2L,
    ifelse(agg$pct > 0, 1L, 0L)))))

  agg
}

#' Aggregate biovolume data per station visit
#'
#' Aggregates sample volumes, computes per-liter concentrations, and assigns
#' presence categories.
#'
#' @param biovolume_data Output from \code{summarize_biovolumes()}.
#' @param metadata Station-matched metadata with \code{STATION_NAME},
#'   \code{COAST}, \code{STATION_NAME_SHORT}, \code{sample_time}, and
#'   \code{ml_analyzed} columns.
#' @return A data.frame with per-station, per-taxon summary including
#'   \code{counts_per_liter}, \code{biovolume_mm3_per_liter},
#'   \code{carbon_ug_per_liter}, and \code{Presence_cat}.
#' @export
aggregate_station_data <- function(biovolume_data, metadata) {
  all_data <- merge(
    biovolume_data,
    metadata[, c("pid", "STATION_NAME", "STATION_NAME_SHORT", "COAST",
                 "sample_time")],
    by.x = "sample",
    by.y = "pid",
    all.x = TRUE
  )

  all_data$sample_date <- as.Date(all_data$sample_time)
  all_data <- assign_station_visits(all_data)

  visit_dates <- compute_visit_dates(all_data)
  all_data <- merge(all_data, visit_dates, by = c("visit_id", "STATION_NAME"),
                    all.x = TRUE)

  sample_volume <- compute_sample_volumes(all_data)

  agg <- stats::aggregate(
    cbind(total_counts = counts,
          total_biovolume_mm3 = biovolume_mm3,
          total_carbon_ug = carbon_ug) ~
      visit_id + STATION_NAME + STATION_NAME_SHORT + COAST + visit_date +
      name + AphiaID,
    data = all_data,
    FUN = sum,
    na.rm = TRUE
  )

  agg <- merge(agg, sample_volume, by = c("visit_id", "STATION_NAME"),
               all.x = TRUE)

  agg <- compute_per_liter(agg)

  # Join station coordinates
  station_list <- load_shark_stations(verbose = FALSE)
  station_coords <- unique(station_list[, c("STATION_NAME",
                                            "LATITUDE_WGS84_SWEREF99_DD",
                                            "LONGITUDE_WGS84_SWEREF99_DD")])
  agg <- merge(agg, station_coords, by = "STATION_NAME", all.x = TRUE)

  compute_presence_categories(agg)
}

#' Collect ferrybox data for station visits
#'
#' @param sample_times POSIXct vector of sample timestamps.
#' @param ferrybox_path Path to ferrybox data folder.
#' @return A data.frame with chlorophyll and other measurements per timestamp.
#' @export
collect_ferrybox_data <- function(sample_times, ferrybox_path) {
  if (!nzchar(ferrybox_path) || !dir.exists(ferrybox_path)) {
    return(data.frame(
      timestamp = sample_times[0],
      chl = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # SEATRACK parameter codes for ferrybox sensors. Each "8xxxx" code is a
  # measurement parameter; the corresponding "88xxxx" is its QC flag (1 = good).
  # Key parameter: 8063 = chlorophyll fluorescence (ug/L), 88063 = its QC flag.
  ferrybox_parameters <- c("70", "80070", "8063", "88063", "8165", "88165",
                           "8173", "88173", "8166", "88166", "8172", "88172",
                           "8174", "88174", "8177", "88177", "8179", "88179",
                           "8181", "88181", "8190", "88190", "8191", "88191")

  fb_data <- iRfcb::ifcb_get_ferrybox_data(
    unique(sample_times),
    ferrybox_folder = ferrybox_path,
    parameters = ferrybox_parameters
  )

  # Extract chlorophyll fluorescence (parameter 8063), keeping only values
  # where the QC flag (parameter 88063) equals 1 (= quality approved)
  if (nrow(fb_data) > 0 && "8063" %in% names(fb_data)) {
    fb_data$chl <- ifelse(
      !is.na(fb_data[["88063"]]) & fb_data[["88063"]] == 1,
      fb_data[["8063"]], NA_real_
    )
  } else {
    fb_data$chl <- NA_real_
  }

  fb_data
}

#' Create wide-format summary for a region
#'
#' @param station_summary Aggregated station data from
#'   \code{aggregate_station_data()}.
#' @param coast "EAST" for Baltic Sea or "WEST" for West Coast.
#' @return A wide-format data.frame with scientific names as rows and
#'   station-date as columns.
#' @export
create_wide_summary <- function(station_summary, coast) {
  region_data <- station_summary[station_summary$COAST == coast, ]

  if (nrow(region_data) == 0) {
    return(data.frame(scientific_name = character(0)))
  }

  region_data$station_date <- paste(region_data$STATION_NAME_SHORT,
                                    region_data$visit_date, sep = "_")

  wide <- tidyr::pivot_wider(
    region_data[, c("name", "station_date", "biovolume_mm3_per_liter")],
    names_from = "station_date",
    values_from = "biovolume_mm3_per_liter"
  )
  names(wide)[1] <- "scientific_name"

  # Order columns by date then station
  data_cols <- names(wide)[-1]
  if (length(data_cols) > 0) {
    parts <- data.frame(
      col = data_cols,
      date = as.Date(sub(".*_", "", data_cols)),
      station = sub("_[^_]+$", "", data_cols),
      stringsAsFactors = FALSE
    )
    parts <- parts[order(parts$date, parts$station), ]
    wide <- wide[, c("scientific_name", parts$col)]
  }

  wide
}

#' Apply class invalidation
#'
#' Replaces invalidated class names with "unclassified" in the classification
#' data.
#'
#' @param classifications A data.frame with a \code{class_name} column.
#' @param invalidated_classes Character vector of class names to invalidate.
#' @return A new data.frame with invalidated classes set to "unclassified".
#' @keywords internal
apply_invalidation <- function(classifications, invalidated_classes) {
  result <- classifications
  result$class_name <- ifelse(
    result$class_name %in% invalidated_classes,
    "unclassified",
    result$class_name
  )
  result
}
