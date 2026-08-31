# Seed / refresh the is_diatom column of inst/extdata/taxa_lookup.csv
#
# The column selects the Menden-Deuer & Lessard carbon conversion formula
# (diatom vs non-diatom) and is the primary, offline source for the per-class
# diatom decision; WoRMS is only consulted at runtime for classes missing
# from the lookup. Re-run this script after adding new rows to the CSV, then
# review the diff before committing:
#
#   Rscript data-raw/seed_taxa_lookup_is_diatom.R
#
# WoRMS fuzzy name matching is imperfect (homonym genera such as
# Actinocyclus -- a diatom, but also a nudibranch -- can come back FALSE, and
# non-taxon classes occasionally match something spurious), so the WoRMS
# answer is combined with the curated genus pattern list in
# identify_diatom_classes() and any disagreement must be checked by hand.

devtools::load_all(quiet = TRUE)

csv_path <- "inst/extdata/taxa_lookup.csv"
lookup <- read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8")

# Query WoRMS in small chunks so one failure does not abort the whole run
chunks <- split(lookup$clean_names, ceiling(seq_along(lookup$clean_names) / 25))
worms <- unlist(lapply(chunks, function(chunk) {
  Sys.sleep(1)
  tryCatch(
    as.logical(iRfcb::ifcb_is_diatom(chunk, verbose = FALSE)),
    error = function(e) {
      warning("WoRMS lookup failed for a chunk: ", conditionMessage(e),
              call. = FALSE)
      rep(NA, length(chunk))
    }
  )
}), use.names = FALSE)

# Pattern list handles WoRMS homonym misses (e.g. Actinocyclus); strip the
# is_diatom column first so identify_diatom_classes() uses the patterns
patterns <- identify_diatom_classes(lookup[, setdiff(names(lookup),
                                                     "is_diatom")])

previous <- if ("is_diatom" %in% names(lookup)) lookup$is_diatom else NA
seeded <- (!is.na(worms) & worms) | lookup$clean_names %in% patterns
disagree <- lookup$clean_names[xor(!is.na(worms) & worms,
                                   lookup$clean_names %in% patterns)]
if (length(disagree) > 0) {
  message("WoRMS and the genus pattern list disagree (union used, ",
          "review manually):\n  ", paste(disagree, collapse = "\n  "))
}
changed <- which(!is.na(previous) & previous != seeded)
if (length(changed) > 0) {
  message("Changed relative to the committed column (review manually):\n  ",
          paste(lookup$clean_names[changed], collapse = "\n  "))
}

lookup$is_diatom <- seeded

# The file is written unquoted; refuse fields that would need quoting
stopifnot(!any(vapply(lookup, function(col) any(grepl("[,\"]", col)),
                      logical(1))))
write.csv(lookup, csv_path, row.names = FALSE, quote = FALSE,
          fileEncoding = "UTF-8", na = "")
message("Wrote ", csv_path, ": ", nrow(lookup), " rows, ",
        sum(lookup$is_diatom), " diatoms")
message("Note: empty AphiaID cells are written as blank; the committed file ",
        "uses literal NA for a few rows -- check the git diff.")
