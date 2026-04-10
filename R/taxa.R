#' Load phytoplankton group configuration from YAML
#'
#' Reads \code{inst/config/phyto_groups.yaml} and returns a list with two
#' elements: \code{core} (named list of class/phylum vectors for the three
#' built-in SHARK4R groups) and \code{custom} (named list suitable for the
#' \code{custom_groups} argument of
#' \code{SHARK4R::assign_phytoplankton_group()}).
#'
#' @return Named list with elements \code{core} and \code{custom}.
#' @keywords internal
load_phyto_group_config <- function() {
  path <- system.file("config", "phyto_groups.yaml", package = "algaware")
  if (!nzchar(path)) {
    stop("inst/config/phyto_groups.yaml not found in algaware package")
  }
  cfg <- yaml::read_yaml(path)

  role_map <- list(
    diatoms        = "Diatoms",
    dinoflagellates = "Dinoflagellates",
    cyanobacteria  = "Cyanobacteria"
  )

  core   <- list()
  custom <- list()

  for (group_name in names(cfg)) {
    grp  <- cfg[[group_name]]
    role <- grp[["role"]]
    criteria <- grp[setdiff(names(grp), "role")]

    if (!is.null(role) && role %in% names(role_map)) {
      core[[role]] <- criteria
    } else {
      custom[[group_name]] <- criteria
    }
  }

  list(core = core, custom = custom)
}

#' Assign phytoplankton groups using the bundled YAML configuration
#'
#' Thin wrapper around \code{SHARK4R::assign_phytoplankton_group()} that
#' reads group definitions from \code{inst/config/phyto_groups.yaml} so
#' class/phylum mappings do not need to be hardcoded in application code.
#'
#' @param scientific_names Character vector of scientific names.
#' @param aphia_ids Integer vector of AphiaIDs (same length), or \code{NULL}.
#' @param verbose Passed to \code{SHARK4R::assign_phytoplankton_group()}.
#' @return Return value of \code{SHARK4R::assign_phytoplankton_group()}.
#' @export
assign_phyto_groups <- function(scientific_names, aphia_ids = NULL,
                                verbose = FALSE) {
  cfg <- load_phyto_group_config()

  diatom_cfg  <- cfg$core[["diatoms"]]
  dino_cfg    <- cfg$core[["dinoflagellates"]]
  cyano_cfg   <- cfg$core[["cyanobacteria"]]

  SHARK4R::assign_phytoplankton_group(
    scientific_names     = scientific_names,
    aphia_ids            = aphia_ids,
    diatom_class         = diatom_cfg[["class"]]   %||% character(0),
    dinoflagellate_class = dino_cfg[["class"]]     %||% character(0),
    cyanobacteria_class  = cyano_cfg[["class"]]    %||% character(0),
    cyanobacteria_phylum = cyano_cfg[["phylum"]]   %||% character(0),
    custom_groups        = cfg$custom,
    verbose              = verbose
  )
}

#' Load the bundled taxa lookup table
#'
#' Returns the pre-built mapping from classifier class names to WoRMS
#' scientific names and AphiaIDs.
#'
#' @return A data.frame with columns \code{clean_names}, \code{name},
#'   \code{AphiaID}.
#' @export
load_taxa_lookup <- function() {
  lookup_file <- system.file("extdata", "taxa_lookup.csv",
                             package = "algaware")
  utils::read.csv(lookup_file, stringsAsFactors = FALSE)
}

#' Build Grouped Relabel Choices
#'
#' Merges the database class list, taxa lookup, and custom classes into
#' a grouped list suitable for \code{selectizeInput} with optgroups.
#' Database classes appear first, then taxa lookup classes not already
#' in the database, then custom classes.
#'
#' @param db_class_list Character vector of class names from the global
#'   class list (database).
#' @param taxa_lookup Data frame with at least a \code{clean_names} column.
#' @param custom_classes Data frame with at least a \code{clean_names} column.
#' @return A named list with two elements: \code{grouped} (a named list
#'   of character vectors for selectize optgroups) and \code{all} (a flat
#'   character vector of all unique class names).
#' @keywords internal
build_relabel_choices <- function(db_class_list = character(0),
                                  taxa_lookup = NULL,
                                  custom_classes = NULL) {
  db_classes <- sort(setdiff(db_class_list, "unclassified"))

  taxa_classes <- character(0)
  if (!is.null(taxa_lookup) && nrow(taxa_lookup) > 0) {
    taxa_classes <- sort(setdiff(
      taxa_lookup$clean_names,
      c(db_classes, "unclassified", "")
    ))
  }

  custom <- character(0)
  if (!is.null(custom_classes) && nrow(custom_classes) > 0) {
    custom <- sort(setdiff(
      custom_classes$clean_names,
      c(db_classes, taxa_classes, "unclassified", "")
    ))
  }

  grouped <- list()
  if (length(db_classes) > 0) grouped[["Database classes"]] <- db_classes
  if (length(taxa_classes) > 0) grouped[["Taxa lookup"]] <- taxa_classes
  if (length(custom) > 0) grouped[["Custom classes"]] <- custom
  grouped[["Other"]] <- "unclassified"

  all_classes <- c(db_classes, taxa_classes, custom, "unclassified")

  list(grouped = grouped, all = all_classes)
}

#' Merge Custom Classes into Taxa Lookup
#'
#' Appends custom class entries to a taxa lookup data frame for use
#' in report generation. Only adds classes not already present.
#'
#' @param taxa_lookup Data frame with columns \code{clean_names},
#'   \code{name}, \code{AphiaID}, \code{HAB}, \code{warning_level},
#'   \code{italic}.
#' @param custom_classes Data frame with the same columns (except
#'   \code{warning_level}) plus \code{is_diatom}. Custom entries
#'   receive \code{warning_level = NA}.
#' @return A new data frame combining both inputs (without duplicates).
#' @export
merge_custom_taxa <- function(taxa_lookup, custom_classes) {
  if (is.null(custom_classes) || nrow(custom_classes) == 0) {
    return(taxa_lookup)
  }

  keep_cols <- intersect(
    c("clean_names", "name", "sflag", "AphiaID", "HAB", "italic"),
    names(custom_classes)
  )

  new_entries <- custom_classes[
    !custom_classes$clean_names %in% taxa_lookup$clean_names,
    keep_cols,
    drop = FALSE
  ]

  if (nrow(new_entries) == 0) return(taxa_lookup)

  # Ensure sflag and warning_level columns exist in both before binding.
  # Custom classes never carry warning levels, so they receive NA.
  if (!"sflag" %in% names(taxa_lookup)) taxa_lookup$sflag <- ""
  if (!"sflag" %in% names(new_entries)) new_entries$sflag <- ""
  if (!"warning_level" %in% names(taxa_lookup)) taxa_lookup$warning_level <- NA_real_
  if (!"warning_level" %in% names(new_entries)) new_entries$warning_level <- NA_real_

  rbind(taxa_lookup[, union(names(taxa_lookup), names(new_entries))],
        new_entries[, union(names(taxa_lookup), names(new_entries))])
}

#' Format taxon labels with italic and sflag for HTML (ggtext) rendering
#'
#' Builds display labels for a vector of scientific names (name + sflag
#' combined). Italic names are wrapped in \code{<i>...</i>}; the sflag
#' suffix is always plain text.
#'
#' @param scientific_names Character vector of display names to format.
#' @param taxa_lookup Data frame with columns \code{name}, \code{sflag},
#'   and \code{italic}.
#' @return Named character vector of HTML-formatted labels, same length and
#'   names as \code{scientific_names}.
#' @keywords internal
#' @param format \code{"html"} (default) wraps the name in \code{<i>...</i>}
#'   for ggtext rendering; \code{"plain"} returns the display name unchanged.
format_taxon_labels <- function(scientific_names, taxa_lookup,
                                format = c("html", "plain")) {
  format <- match.arg(format)

  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0 || format == "plain") {
    return(setNames(scientific_names, scientific_names))
  }

  sflag <- if ("sflag" %in% names(taxa_lookup)) taxa_lookup$sflag else rep("", nrow(taxa_lookup))
  sflag[is.na(sflag)] <- ""
  display_names <- trimws(paste(taxa_lookup$name, sflag))
  italic <- if ("italic" %in% names(taxa_lookup)) taxa_lookup$italic else rep(FALSE, nrow(taxa_lookup))

  lookup <- data.frame(
    display_name = display_names,
    name         = taxa_lookup$name,
    sflag        = sflag,
    italic       = italic,
    stringsAsFactors = FALSE
  )
  lookup <- lookup[!duplicated(lookup$display_name), ]

  labels <- vapply(scientific_names, function(dn) {
    idx <- match(dn, lookup$display_name)
    if (is.na(idx) || !isTRUE(lookup$italic[idx])) return(dn)
    name_html <- paste0("<i>", lookup$name[idx], "</i>")
    if (nzchar(lookup$sflag[idx])) paste(name_html, lookup$sflag[idx]) else name_html
  }, character(1))

  setNames(labels, scientific_names)
}

#' Enrich a corrections data frame with custom class metadata
#'
#' Appends columns describing any custom class referenced in the
#' \code{new_class} column so that a corrections CSV is self-contained
#' and can be re-imported to reconstruct custom classes.  Rows whose
#' \code{new_class} is not in \code{custom_classes} receive \code{NA}
#' in all added columns.
#'
#' @param corrections Data frame with at least a \code{new_class} column.
#' @param custom_classes Data frame of custom classes (from \code{rv$custom_classes}).
#' @return \code{corrections} with extra columns \code{custom_sci_name},
#'   \code{custom_sflag}, \code{custom_aphia_id}, \code{custom_hab},
#'   \code{custom_italic}.
#' @keywords internal
enrich_corrections_for_export <- function(corrections, custom_classes) {
  corrections$custom_sci_name  <- NA_character_
  corrections$custom_sflag     <- NA_character_
  corrections$custom_aphia_id  <- NA_integer_
  corrections$custom_hab       <- NA
  corrections$custom_italic    <- NA

  if (is.null(custom_classes) || nrow(custom_classes) == 0) {
    return(corrections)
  }

  custom_idx <- match(corrections$new_class, custom_classes$clean_names)
  has_custom <- !is.na(custom_idx)
  if (!any(has_custom)) return(corrections)

  idx <- custom_idx[has_custom]
  corrections$custom_sci_name[has_custom] <- custom_classes$name[idx]
  corrections$custom_sflag[has_custom]    <-
    if ("sflag" %in% names(custom_classes)) custom_classes$sflag[idx] else ""
  corrections$custom_aphia_id[has_custom] <- custom_classes$AphiaID[idx]
  corrections$custom_hab[has_custom]      <- custom_classes$HAB[idx]
  corrections$custom_italic[has_custom]   <- custom_classes$italic[idx]

  corrections
}
