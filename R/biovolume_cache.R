#' Per-ROI biovolume cache
#'
#' `summarize_biovolumes()` re-reads every feature CSV and .hdr file from disk
#' each time it runs, even though relabelling ROIs only changes the class
#' labels: the per-ROI biovolume (from the feature files) and the per-sample
#' analysed volume (from the .hdr files) never change after download. These
#' functions read that immutable data once at load time and cache it in memory
#' (`rv$biovolume_cache`), so summary recomputation -- after corrections,
#' sample exclusions, or at report time -- becomes an in-memory aggregation
#' instead of a full re-read of the storage folder.
#'
#' The per-class diatom decision (which selects the carbon conversion formula)
#' comes from the curated `is_diatom` column of the taxa lookup; classes not
#' covered there fall back to a WoRMS API lookup, cached per class in
#' `rv$diatom_status` so only classes not seen before in the session trigger
#' a network request.
#'
#' @name biovolume-cache
#' @keywords internal
NULL

#' Derive sample IDs from feature file names
#'
#' Feature files are named \code{<pid>_fea_vN.csv} (IFCB convention, matched
#' by iRfcb) or \code{<pid>_features.csv}; strip either suffix to recover the
#' bare sample ID.
#'
#' @param files Character vector of feature file paths or basenames.
#' @return Character vector of sample IDs.
#' @keywords internal
feature_sample_name <- function(files) {
  base <- basename(files)
  s <- sub("_fe[a-z]*_v\\d+\\.csv$", "", base)
  s <- sub("_features\\.csv$", "", s)
  sub("\\.csv$", "", s)
}

#' Read analysed volume for one sample, returning NA on failure
#'
#' Wraps \code{iRfcb::ifcb_volume_analyzed()} (which needs the .adc file next
#' to the .hdr file) so that one corrupt or incomplete sample yields an NA
#' volume -- and therefore NA per-liter values downstream -- instead of
#' aborting the whole summary.
#'
#' @param hdr_file Path to a .hdr file.
#' @return Numeric ml analysed, or NA.
#' @keywords internal
volume_analyzed_safe <- function(hdr_file) {
  tryCatch(
    as.numeric(iRfcb::ifcb_volume_analyzed(hdr_file))[1],
    error = function(e) {
      warning("Failed to read analysed volume from ", basename(hdr_file),
              ": ", conditionMessage(e), call. = FALSE)
      NA_real_
    }
  )
}

#' Read per-sample analysed volumes from .hdr files
#'
#' @param hdr_folder Directory containing .hdr (and .adc) files.
#' @param sample_ids Character vector of sample IDs to read.
#' @return A data.frame with \code{sample} and \code{ml_analyzed} columns.
#' @keywords internal
read_sample_volumes <- function(hdr_folder, sample_ids) {
  hdr_files <- list.files(hdr_folder, pattern = "D.*\\.hdr$",
                          full.names = TRUE, recursive = TRUE)
  hdr_samples <- tools::file_path_sans_ext(basename(hdr_files))
  keep <- hdr_samples %in% sample_ids
  hdr_files <- hdr_files[keep]
  hdr_samples <- hdr_samples[keep]

  data.frame(
    sample = hdr_samples,
    ml_analyzed = vapply(hdr_files, volume_analyzed_safe, numeric(1),
                         USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Build the per-ROI biovolume cache from downloaded files
#'
#' Reads all feature CSVs and .hdr files for the given samples once. The raw
#' pixel biovolume is stored (not micron-converted) so a later change to the
#' pixels-per-micron setting still takes effect at summary time.
#'
#' @param feature_folder Path to the feature CSV directory.
#' @param hdr_folder Path to the raw data directory (for .hdr/.adc files).
#' @param sample_ids Character vector of sample IDs to include.
#' @return A list with \code{roi_biovolumes} (data.frame: \code{sample},
#'   \code{roi_number}, \code{biovolume_px}) and \code{sample_volumes}
#'   (data.frame: \code{sample}, \code{ml_analyzed}).
#' @export
build_biovolume_cache <- function(feature_folder, hdr_folder, sample_ids) {
  feature_files <- list.files(feature_folder, pattern = "D.*\\.csv",
                              full.names = TRUE, recursive = TRUE)
  feature_files <- feature_files[!grepl("multiblob", basename(feature_files),
                                        ignore.case = TRUE)]
  feature_files <- feature_files[feature_sample_name(feature_files) %in%
                                   sample_ids]

  empty_rois <- data.frame(sample = character(0), roi_number = integer(0),
                           biovolume_px = numeric(0), stringsAsFactors = FALSE)

  roi_biovolumes <- empty_rois
  if (length(feature_files) > 0) {
    features <- iRfcb::ifcb_read_features(feature_files,
                                          biovolume_only = TRUE,
                                          verbose = FALSE)
    roi_list <- lapply(names(features), function(file_name) {
      file_data <- features[[file_name]]
      if (is.null(file_data) || nrow(file_data) == 0) return(NULL)
      data.frame(
        sample = feature_sample_name(file_name),
        roi_number = as.integer(file_data$roi_number),
        biovolume_px = as.numeric(file_data$Biovolume),
        stringsAsFactors = FALSE
      )
    })
    roi_list <- Filter(Negate(is.null), roi_list)
    if (length(roi_list) > 0) {
      roi_biovolumes <- do.call(rbind, roi_list)
    }
  }

  list(
    roi_biovolumes = roi_biovolumes,
    sample_volumes = read_sample_volumes(hdr_folder, sample_ids)
  )
}

#' Look up diatom status in WoRMS
#'
#' Thin wrapper around \code{iRfcb::ifcb_is_diatom()} so tests can mock the
#' network call. Returns NA for every class when the lookup fails (e.g.
#' offline), so failed classes are retried on the next call rather than being
#' permanently cached as non-diatoms.
#'
#' @param class_names Character vector of class names.
#' @return Logical vector (TRUE/FALSE from WoRMS, NA on lookup failure).
#' @keywords internal
worms_diatom_lookup <- function(class_names) {
  tryCatch(
    as.logical(iRfcb::ifcb_is_diatom(class_names, verbose = FALSE)),
    error = function(e) {
      warning("WoRMS diatom lookup failed (", conditionMessage(e),
              "); affected classes (those without an is_diatom flag in the ",
              "taxa lookup) are treated as non-diatoms unless matched by ",
              "the local diatom genus list.", call. = FALSE)
      rep(NA, length(class_names))
    }
  )
}

#' Resolve per-class diatom status, reusing cached lookups
#'
#' Resolution order per class: the curated \code{is_diatom} column of the
#' taxa lookup (no network), then a previously resolved status table, then a
#' WoRMS lookup for classes neither source covers (e.g. classifier classes
#' added after the bundled lookup was last updated). Classes whose earlier
#' WoRMS lookup failed (NA) are retried. The local diatom genus list
#' (\code{identify_diatom_classes()}) is NOT applied here -- it can change
#' within a session as custom classes are added, so it is applied at summary
#' time instead.
#'
#' @param class_names Character vector of class names in use.
#' @param cached_status Optional data.frame from a previous call, with
#'   \code{class} and \code{worms_diatom} columns.
#' @param taxa_lookup Optional taxa lookup data.frame; when it has a logical
#'   \code{is_diatom} column, classes with a non-NA flag are resolved from it
#'   without any WoRMS lookup.
#' @return A data.frame with \code{class} and \code{worms_diatom} (logical,
#'   NA when the lookup failed) covering all \code{class_names}.
#' @export
resolve_diatom_status <- function(class_names, cached_status = NULL,
                                  taxa_lookup = NULL) {
  classes <- unique(class_names[!is.na(class_names)])

  known <- data.frame(class = character(0), worms_diatom = logical(0),
                      stringsAsFactors = FALSE)
  if (!is.null(taxa_lookup) && "is_diatom" %in% names(taxa_lookup)) {
    flag <- as.logical(taxa_lookup$is_diatom)
    keep <- !is.na(flag) & taxa_lookup$clean_names %in% classes
    known <- data.frame(class = taxa_lookup$clean_names[keep],
                        worms_diatom = flag[keep],
                        stringsAsFactors = FALSE)
  }
  if (!is.null(cached_status) && nrow(cached_status) > 0) {
    cached <- cached_status[!is.na(cached_status$worms_diatom) &
                              cached_status$class %in% classes &
                              !cached_status$class %in% known$class, ,
                            drop = FALSE]
    known <- rbind(known, cached)
  }

  new_classes <- setdiff(classes, known$class)
  if (length(new_classes) > 0) {
    known <- rbind(known, data.frame(
      class = new_classes,
      worms_diatom = worms_diatom_lookup(new_classes),
      stringsAsFactors = FALSE
    ))
  }

  known
}

#' Carbon conversion for large diatoms (Menden-Deuer & Lessard 2000)
#'
#' Mirrors iRfcb's default diatom equation (\code{diatom_equation = "large"});
#' a unit test asserts the two stay identical.
#'
#' @param volume_um3 Biovolume in cubic microns.
#' @return Carbon in picograms.
#' @keywords internal
vol2c_diatom_large <- function(volume_um3) {
  10^(-0.933 + 0.881 * log10(volume_um3))
}

#' Carbon conversion for non-diatom protists (Menden-Deuer & Lessard 2000)
#'
#' Mirrors iRfcb's non-diatom equation; a unit test asserts the two stay
#' identical.
#'
#' @param volume_um3 Biovolume in cubic microns.
#' @return Carbon in picograms.
#' @keywords internal
vol2c_nondiatom <- function(volume_um3) {
  10^(-0.665 + 0.939 * log10(volume_um3))
}

#' Summarize biovolumes from the in-memory cache
#'
#' Reproduces the output of \code{summarize_biovolumes()} (which wraps
#' \code{iRfcb::ifcb_summarize_biovolumes()}) without touching the disk:
#' per-ROI biovolumes are joined with the (possibly corrected) classifications,
#' carbon is converted per ROI with the diatom or non-diatom formula, and the
#' result is aggregated per sample and class.
#'
#' ROIs without a classification row -- including all ROIs of samples the user
#' has excluded -- are dropped, matching the file-based path where such rows
#' either never enter (excluded samples) or are discarded downstream by the
#' name-based aggregation (NA class).
#'
#' @param cache List from \code{build_biovolume_cache()}.
#' @param classifications Data.frame with \code{sample_name},
#'   \code{roi_number}, \code{class_name} (from
#'   \code{read_h5_classifications()}, possibly corrected).
#' @param taxa_lookup Taxa lookup data.frame (see
#'   \code{summarize_biovolumes()}).
#' @param non_bio_classes Character vector of non-biological classes to
#'   exclude.
#' @param pixels_per_micron Conversion factor from pixels to microns.
#' @param diatom_status Optional data.frame from
#'   \code{resolve_diatom_status()}; resolved on the fly (WoRMS lookup) when
#'   NULL.
#' @param custom_classes Optional data frame of custom classes with an
#'   \code{is_diatom} column (extends the local diatom genus list).
#' @return A data.frame with the same columns as
#'   \code{summarize_biovolumes()}.
#' @export
summarize_biovolumes_cached <- function(cache, classifications, taxa_lookup,
                                        non_bio_classes = character(0),
                                        pixels_per_micron = 2.77,
                                        diatom_status = NULL,
                                        custom_classes = NULL) {
  if (is.null(diatom_status)) {
    diatom_status <- resolve_diatom_status(unique(classifications$class_name),
                                           taxa_lookup = taxa_lookup)
  }

  micron_factor <- 1 / pixels_per_micron

  joined <- merge(
    cache$roi_biovolumes,
    data.frame(sample = classifications$sample_name,
               roi_number = classifications$roi_number,
               class = classifications$class_name,
               stringsAsFactors = FALSE),
    by = c("sample", "roi_number"),
    all.x = TRUE
  )
  joined <- joined[!is.na(joined$class), , drop = FALSE]

  empty <- data.frame(
    sample = character(0), classifier = character(0), class = character(0),
    counts = integer(0), biovolume_mm3 = numeric(0), carbon_ug = numeric(0),
    ml_analyzed = numeric(0), counts_per_liter = numeric(0),
    biovolume_mm3_per_liter = numeric(0), carbon_ug_per_liter = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(joined) == 0) {
    return(finalize_biovolume_data(empty, taxa_lookup, non_bio_classes))
  }

  joined$biovolume_um3 <- joined$biovolume_px * micron_factor^3

  # Diatom status: resolved per class from the taxa lookup or WoRMS (NA =
  # lookup failed -> non-diatom), extended by the local genus pattern list
  # and custom classes flagged as diatoms -- the same override iRfcb applies
  # via its diatom_include argument.
  diatom_include <- identify_diatom_classes(taxa_lookup, custom_classes)
  worms <- diatom_status$worms_diatom[match(joined$class, diatom_status$class)]
  is_diatom <- (!is.na(worms) & worms) | joined$class %in% diatom_include

  joined$carbon_pg <- vol2c_nondiatom(joined$biovolume_um3)
  joined$carbon_pg[is_diatom] <-
    vol2c_diatom_large(joined$biovolume_um3[is_diatom])

  # Same aggregation as iRfcb::ifcb_summarize_biovolumes(): counts include
  # ROIs with NA biovolume, while the sums skip the NAs (na.pass + na.rm).
  sums <- stats::aggregate(
    cbind(biovolume_um3 = biovolume_um3, carbon_pg = carbon_pg) ~
      sample + class,
    data = joined,
    FUN = function(x) sum(x, na.rm = TRUE),
    na.action = stats::na.pass
  )
  counts <- stats::aggregate(
    cbind(counts = roi_number) ~ sample + class,
    data = joined,
    FUN = length,
    na.action = stats::na.pass
  )

  agg <- merge(counts, sums, by = c("sample", "class"))
  agg$classifier <- NA_character_
  agg$biovolume_mm3 <- agg$biovolume_um3 * 1e-9
  agg$carbon_ug <- agg$carbon_pg * 1e-6

  agg <- merge(agg, cache$sample_volumes, by = "sample", all.x = TRUE)
  ml_liters <- agg$ml_analyzed / 1000
  agg$counts_per_liter <- agg$counts / ml_liters
  agg$biovolume_mm3_per_liter <- agg$biovolume_mm3 / ml_liters
  agg$carbon_ug_per_liter <- agg$carbon_ug / ml_liters

  agg <- agg[, names(empty)]

  finalize_biovolume_data(agg, taxa_lookup, non_bio_classes)
}

#' Recompute biovolume data, using the session cache when available
#'
#' Central entry point for summary recomputation (report generation, sample
#' exclusions, stale-summary refresh). Uses the in-memory per-ROI cache built
#' at load time; falls back to the file-based \code{summarize_biovolumes()}
#' when no cache exists. Updates \code{rv$diatom_status} with any newly
#' resolved classes as a side effect.
#'
#' @param rv Reactive values (or list-like) with \code{classifications},
#'   \code{custom_classes}, \code{biovolume_cache}, \code{diatom_status}.
#' @param config Reactive values (or list-like) with
#'   \code{local_storage_path} and \code{pixels_per_micron}.
#' @param taxa_lookup Taxa lookup data.frame (already merged with custom
#'   taxa where applicable).
#' @param non_bio_classes Character vector of non-biological classes.
#' @return A data.frame as returned by \code{summarize_biovolumes()}.
#' @export
recompute_biovolume_data <- function(rv, config, taxa_lookup,
                                     non_bio_classes = character(0)) {
  cache <- rv$biovolume_cache
  if (is.null(cache) || is.null(cache$roi_biovolumes) ||
      nrow(cache$roi_biovolumes) == 0) {
    storage <- config$local_storage_path
    return(summarize_biovolumes(
      file.path(storage, "features"),
      file.path(storage, "raw"),
      rv$classifications, taxa_lookup, non_bio_classes,
      pixels_per_micron = config$pixels_per_micron,
      custom_classes = rv$custom_classes
    ))
  }

  status <- resolve_diatom_status(unique(rv$classifications$class_name),
                                  cached_status = rv$diatom_status,
                                  taxa_lookup = taxa_lookup)
  rv$diatom_status <- status

  summarize_biovolumes_cached(
    cache, rv$classifications, taxa_lookup,
    non_bio_classes = non_bio_classes,
    pixels_per_micron = config$pixels_per_micron,
    diatom_status = status,
    custom_classes = rv$custom_classes
  )
}
