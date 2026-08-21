# Tests for the AlgAware-specific create_group_map() wrapper. The underlying
# create_pie_map() and its helpers live in (and are tested by) SHARK4R.

group_map_inputs <- function() {
  station_summary <- data.frame(
    name = c("Mesodinium", "Skeletonema marinoi"),
    AphiaID = c(179320L, 149142L),
    carbon_ug_per_liter = c(10, 20),
    STATION_NAME_SHORT = c("BY5", "BY31"),
    LATITUDE_WGS84_SWEREF99_DD = c(55.25, 58.59),
    LONGITUDE_WGS84_SWEREF99_DD = c(15.98, 18.23),
    stringsAsFactors = FALSE
  )
  phyto_groups <- data.frame(
    name = c("Mesodinium", "Skeletonema marinoi"),
    AphiaID = c(179320L, 149142L),
    phyto_group.plankton_group = c("Mesodinium spp.", "Diatoms"),
    stringsAsFactors = FALSE
  )
  list(station_summary = station_summary, phyto_groups = phyto_groups)
}

test_that("create_group_map returns a ggplot", {
  skip_if_not_installed("rnaturalearthdata")
  io <- group_map_inputs()
  p <- create_group_map(io$station_summary, io$phyto_groups)
  expect_s3_class(p, "ggplot")
})

test_that("create_group_map formats Mesodinium legend label with partial italics", {
  skip_if_not_installed("rnaturalearthdata")
  io <- group_map_inputs()
  p <- create_group_map(io$station_summary, io$phyto_groups)
  fill_scale <- p$scales$get_scales("fill")
  expect_match(unname(fill_scale$labels["Mesodinium spp."]),
               "<i>Mesodinium</i> spp.", fixed = TRUE)
  expect_s3_class(p$theme$legend.text, "element_markdown")
})

test_that("create_group_map omits the biomass size legend", {
  skip_if_not_installed("rnaturalearthdata")
  io <- group_map_inputs()
  p <- create_group_map(io$station_summary, io$phyto_groups)
  size_scale <- p$scales$get_scales("size")
  expect_null(size_scale)
})

test_that("create_group_map folds non-pie groups (e.g. Ciliates) into Other", {
  skip_if_not_installed("rnaturalearthdata")
  io <- group_map_inputs()
  io$phyto_groups$phyto_group.plankton_group[1] <- "Ciliates"
  p <- create_group_map(io$station_summary, io$phyto_groups)
  expect_s3_class(p, "ggplot")
  groups <- unlist(lapply(p$layers, function(l) {
    if (!is.null(l$data) && "group" %in% names(l$data)) {
      as.character(l$data$group)
    }
  }))
  expect_false("Ciliates" %in% groups)
})
