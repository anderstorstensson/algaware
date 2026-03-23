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

  all_classes <- c(db_classes, taxa_classes, custom)

  list(grouped = grouped, all = all_classes)
}

#' Merge Custom Classes into Taxa Lookup
#'
#' Appends custom class entries to a taxa lookup data frame for use
#' in report generation. Only adds classes not already present.
#'
#' @param taxa_lookup Data frame with columns \code{clean_names},
#'   \code{name}, \code{AphiaID}, \code{HAB}, \code{italic}.
#' @param custom_classes Data frame with the same columns plus
#'   \code{is_diatom}.
#' @return A new data frame combining both inputs (without duplicates).
#' @keywords internal
merge_custom_taxa <- function(taxa_lookup, custom_classes) {
  if (is.null(custom_classes) || nrow(custom_classes) == 0) {
    return(taxa_lookup)
  }

  new_entries <- custom_classes[
    !custom_classes$clean_names %in% taxa_lookup$clean_names,
    c("clean_names", "name", "AphiaID", "HAB", "italic"),
    drop = FALSE
  ]

  if (nrow(new_entries) == 0) return(taxa_lookup)

  rbind(taxa_lookup, new_entries)
}
