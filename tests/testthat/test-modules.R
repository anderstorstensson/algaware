test_that("mod_settings_ui returns tagList", {
  ui <- mod_settings_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_data_loader_ui returns tagList", {
  ui <- mod_data_loader_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_gallery_ui returns tagList", {
  ui <- mod_gallery_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_validation_ui returns tagList", {
  ui <- mod_validation_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_report_ui returns tag", {
  ui <- mod_report_ui("test")
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_frontpage_ui returns tag", {
  ui <- mod_frontpage_ui("test")
  expect_s3_class(ui, "shiny.tag")
})
