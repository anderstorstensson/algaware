# Builds a minimal H5 classification fixture with the datasets
# read_h5_classifications() consumes. The real fixture in test_data/ predates
# the YOLO chain counter, so tests exercising the optional cell_count dataset
# write their own files with this helper.
write_test_class_h5 <- function(dir, sample_name, class_names,
                                roi_numbers = seq_along(class_names),
                                cell_count = NULL,
                                classifier_name = "test_classifier") {
  path <- file.path(dir, paste0(sample_name, "_class.h5"))
  h5 <- hdf5r::H5File$new(path, mode = "w")
  on.exit(h5$close_all(), add = TRUE)

  h5[["roi_numbers"]] <- as.integer(roi_numbers)
  h5[["class_name"]] <- class_names
  h5[["class_labels"]] <- unique(class_names)
  h5[["output_scores"]] <- matrix(
    0.9,
    nrow = length(unique(class_names)),
    ncol = length(class_names)
  )
  h5[["classifier_name"]] <- classifier_name
  if (!is.null(cell_count)) {
    h5[["cell_count"]] <- as.integer(cell_count)
  }

  invisible(path)
}
