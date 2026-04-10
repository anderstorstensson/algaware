test_that("strip_markdown removes bold markers", {
  result <- algaware:::strip_markdown("This is **bold** text")
  expect_equal(result, "This is bold text")
})

test_that("strip_markdown removes italic markers", {
  result <- algaware:::strip_markdown("This is *italic* text")
  expect_equal(result, "This is italic text")
})

test_that("strip_markdown preserves HAB asterisks after species names", {
  result <- algaware:::strip_markdown("Alexandrium catenella* was observed")
  expect_equal(result, "Alexandrium catenella* was observed")
})

test_that("strip_markdown handles mixed formatting", {
  result <- algaware:::strip_markdown("**Bold** and *italic* with species*")
  expect_equal(result, "Bold and italic with species*")
})

test_that("strip_markdown trims whitespace", {
  result <- algaware:::strip_markdown("  hello  ")
  expect_equal(result, "hello")
})

test_that("format_report_paragraph returns fpar with no taxa_lookup", {
  result <- algaware:::format_report_paragraph("Hello world")
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph returns fpar with empty taxa_lookup", {
  taxa <- data.frame(name = character(0), italic = logical(0),
                     stringsAsFactors = FALSE)
  result <- algaware:::format_report_paragraph("Hello world", taxa)
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph italicizes species names", {
  taxa <- data.frame(
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    italic = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "Skeletonema marinoi was dominant.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_station_data_for_prompt produces expected output", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY31",
    STATION_NAME = "BY31 Landsort Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    biovolume_mm3_per_liter = c(0.5, 0.1),
    carbon_ug_per_liter = c(10.0, 2.0),
    counts_per_liter = c(1000, 200),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data)
  expect_type(result, "character")
  expect_true(grepl("BY31", result))
  expect_true(grepl("Baltic Sea", result))
  expect_true(grepl("Skeletonema marinoi", result))
})

test_that("format_station_data_for_prompt uses detailed and collapsed group assignments", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY31",
    STATION_NAME = "BY31 Landsort Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Skeletonema marinoi", "Teleaulax amphioxeia", "Mesodinium rubrum"),
    AphiaID = c(149142, 157183, 179320),
    biovolume_mm3_per_liter = c(0.5, 0.2, 0.1),
    carbon_ug_per_liter = c(10.0, 4.0, 2.0),
    counts_per_liter = c(1000, 500, 200),
    stringsAsFactors = FALSE
  )
  phyto_groups <- data.frame(
    name = c("Skeletonema marinoi", "Teleaulax amphioxeia", "Mesodinium rubrum"),
    AphiaID = c(149142, 157183, 179320),
    phyto_group = c("Diatoms", "Cryptophytes", "Mesodinium spp."),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data,
    phyto_groups = phyto_groups
  )
  expect_true(grepl("All phytoplankton groups present", result))
  expect_true(grepl("Top 3 groups by biovolume", result))
  expect_true(grepl("Diatoms:", result))
  expect_true(grepl("Cryptophytes:", result))
  expect_true(grepl("Mesodinium spp\\.:", result))
  expect_true(grepl("Teleaulax amphioxeia", result))
  expect_true(grepl("Mesodinium rubrum", result))
})

test_that("ensure_hab_asterisks adds * after missing HAB taxa", {
  taxa <- data.frame(
    name = c("Dinophysis acuminata", "Pseudochattonella", "Aphanizomenon flos-aquae"),
    italic = c(TRUE, TRUE, TRUE),
    HAB = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  text <- "Dinophysis acuminata was observed along with Pseudochattonella spp. and Aphanizomenon flos-aquae."
  result <- algaware:::ensure_hab_asterisks(text, taxa)
  expect_true(grepl("Dinophysis acuminata\\*", result))
  expect_true(grepl("Pseudochattonella spp\\.\\*", result))
  expect_true(grepl("Aphanizomenon flos-aquae\\*", result))
})

test_that("ensure_hab_asterisks does not double-add asterisks", {
  taxa <- data.frame(
    name = "Dinophysis acuminata", italic = TRUE, HAB = TRUE,
    stringsAsFactors = FALSE
  )
  text <- "Dinophysis acuminata* was present."
  result <- algaware:::ensure_hab_asterisks(text, taxa)
  expect_false(grepl("\\*\\*", result))
  expect_true(grepl("Dinophysis acuminata\\*", result))
})

test_that("ensure_hab_asterisks fixes double asterisk from LLM (Genus* spp.*)", {
  taxa <- data.frame(
    name = c("Pseudochattonella", "Pseudo-nitzschia"),
    italic = c(TRUE, TRUE),
    HAB = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  # LLM incorrectly placed * after genus AND after spp.
  text <- "Pseudochattonella* spp.* was dominant. Pseudo-nitzschia* spp.* was also present."
  result <- algaware:::ensure_hab_asterisks(text, taxa)
  expect_true(grepl("Pseudochattonella spp\\.\\*", result))
  expect_false(grepl("Pseudochattonella\\* spp", result))
  expect_true(grepl("Pseudo-nitzschia spp\\.\\*", result))
  expect_false(grepl("Pseudo-nitzschia\\* spp", result))
})

test_that("ensure_hab_asterisks does not add * to genus when spp.* already correct", {
  taxa <- data.frame(
    name = "Pseudochattonella", italic = TRUE, HAB = TRUE,
    stringsAsFactors = FALSE
  )
  text <- "Pseudochattonella spp.* was observed."
  result <- algaware:::ensure_hab_asterisks(text, taxa)
  expect_equal(result, "Pseudochattonella spp.* was observed.")
})

test_that("ensure_hab_asterisks matches abbreviated species names", {
  taxa <- data.frame(
    name = "Dinophysis acuminata", italic = TRUE, HAB = TRUE,
    stringsAsFactors = FALSE
  )
  text <- "D. acuminata was present."
  result <- algaware:::ensure_hab_asterisks(text, taxa)
  expect_true(grepl("D\\. acuminata\\*", result))
})

test_that("ensure_hab_asterisks leaves non-HAB taxa unchanged", {
  taxa <- data.frame(
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    italic = c(TRUE, TRUE),
    HAB = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  text <- "Skeletonema marinoi dominated. Dinophysis acuminata was rare."
  result <- algaware:::ensure_hab_asterisks(text, taxa)
  expect_false(grepl("Skeletonema marinoi\\*", result))
  expect_true(grepl("Dinophysis acuminata\\*", result))
})

test_that("llm_available returns logical", {
  result <- llm_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("load_writing_guide returns non-empty string", {
  result <- algaware:::load_writing_guide()
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("add_formatted_par adds paragraphs to document", {
  doc <- officer::read_docx()
  result <- algaware:::add_formatted_par(doc, "Hello world")
  expect_s3_class(result, "rdocx")
})

test_that("add_formatted_par splits on double newlines", {
  doc <- officer::read_docx()
  result <- algaware:::add_formatted_par(doc, "Para one.\n\nPara two.")
  expect_s3_class(result, "rdocx")
})

test_that("add_formatted_par applies taxa formatting", {
  doc <- officer::read_docx()
  taxa <- data.frame(
    name = "Skeletonema marinoi",
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::add_formatted_par(
    doc, "Skeletonema marinoi was dominant.", taxa
  )
  expect_s3_class(result, "rdocx")
})

test_that("format_report_paragraph handles HAB asterisks", {
  taxa <- data.frame(
    name = c("Alexandrium catenella"),
    italic = c(TRUE),
    HAB = c(TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "Alexandrium catenella* was detected.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph handles abbreviated species", {
  taxa <- data.frame(
    name = c("Skeletonema marinoi"),
    italic = c(TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "S. marinoi was abundant.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_cruise_summary_for_prompt produces text", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY5", "ANHOLT", "ANHOLT"),
    COAST = c("EAST", "EAST", "WEST", "WEST"),
    visit_date = as.Date(c("2024-03-15", "2024-03-15",
                            "2024-03-16", "2024-03-16")),
    visit_id = c("BY5_visit1", "BY5_visit1", "ANHOLT_visit1", "ANHOLT_visit1"),
    name = c("Taxon A", "Taxon B", "Taxon C", "Taxon D"),
    biovolume_mm3_per_liter = c(0.5, 0.1, 0.3, 0.05),
    carbon_ug_per_liter = c(10, 2, 6, 1),
    counts_per_liter = c(1000, 200, 500, 100),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(station_summary)
  expect_type(result, "character")
  expect_true(grepl("BY5", result))
  expect_true(grepl("ANHOLT", result))
})

test_that("format_cruise_summary_for_prompt uses detailed and collapsed group assignments", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY5", "ANHOLT", "ANHOLT"),
    COAST = c("EAST", "EAST", "WEST", "WEST"),
    visit_date = as.Date(c("2024-03-15", "2024-03-15",
                           "2024-03-16", "2024-03-16")),
    visit_id = c("BY5_visit1", "BY5_visit1", "ANHOLT_visit1", "ANHOLT_visit1"),
    name = c("Skeletonema marinoi", "Teleaulax amphioxeia",
             "Karlodinium veneficum", "Mesodinium rubrum"),
    AphiaID = c(149142, 157183, 110690, 179320),
    biovolume_mm3_per_liter = c(0.5, 0.1, 0.3, 0.05),
    carbon_ug_per_liter = c(10, 2, 6, 1),
    counts_per_liter = c(1000, 200, 500, 100),
    stringsAsFactors = FALSE
  )
  phyto_groups <- data.frame(
    name = c("Skeletonema marinoi", "Teleaulax amphioxeia",
             "Karlodinium veneficum", "Mesodinium rubrum"),
    AphiaID = c(149142, 157183, 110690, 179320),
    phyto_group = c("Diatoms", "Cryptophytes", "Dinoflagellates", "Mesodinium spp."),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(
    station_summary,
    phyto_groups = phyto_groups
  )
  expect_true(grepl("Dominant groups: Diatoms", result))
  expect_true(grepl("Dominant groups: Dinoflagellates", result) || grepl("Dinoflagellates 0.300", result))
  expect_true(grepl("Cryptophytes 0.100", result))
  expect_true(grepl("Mesodinium spp\\. 0.050", result))
  expect_true(grepl("Cryptophytes 0.100", result) || grepl("Mesodinium spp\\. 0.050", result))
})

test_that("format_cruise_summary_for_prompt marks HAB species", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_visit1",
    name = "Alexandrium catenella",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Alexandrium catenella",
    HAB = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(
    station_summary, taxa
  )
  expect_true(grepl("HAB", result))
})

test_that("format_station_data_for_prompt handles HAB taxa", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY31",
    STATION_NAME = "BY31 Landsort Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Alexandrium catenella", "Skeletonema marinoi"),
    biovolume_mm3_per_liter = c(0.1, 0.5),
    carbon_ug_per_liter = c(2, 10),
    counts_per_liter = c(200, 1000),
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Alexandrium catenella",
    HAB = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data, taxa)
  expect_true(grepl("\\[HAB\\]", result))
})

test_that("format_station_data_for_prompt includes chl when present", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    chl_mean = 3.14,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data)
  expect_true(grepl("3.14", result))
})

test_that("format_station_data_for_prompt includes unclassified note when >80%", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data, unclassified_pct = 85
  )
  expect_true(grepl("85%", result))
  expect_true(grepl("unclassified", result))
})

test_that("format_station_data_for_prompt omits unclassified note when <=80%", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data, unclassified_pct = 50
  )
  expect_false(grepl("unclassified", result))
})

test_that("format_station_data_for_prompt omits note when pct is NULL", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data, unclassified_pct = NULL
  )
  expect_false(grepl("unclassified", result))
})

test_that("format_cruise_summary_for_prompt includes unclassified tag when >80%", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "ANHOLT"),
    COAST = c("EAST", "WEST"),
    visit_date = as.Date(c("2024-03-15", "2024-03-16")),
    visit_id = c("BY5_visit1", "ANHOLT_visit1"),
    name = c("Taxon A", "Taxon B"),
    biovolume_mm3_per_liter = c(0.5, 0.3),
    carbon_ug_per_liter = c(10, 6),
    counts_per_liter = c(1000, 500),
    stringsAsFactors = FALSE
  )
  fractions <- list(BY5_visit1 = 90, ANHOLT_visit1 = 40)
  result <- algaware:::format_cruise_summary_for_prompt(
    station_summary, unclassified_fractions = fractions
  )
  # BY5 has 90% -> should show UNCLASSIFIED tag
  expect_true(grepl("UNCLASSIFIED: 90%", result))
  # ANHOLT has 40% -> should NOT show tag
  expect_false(grepl("UNCLASSIFIED: 40%", result))
})

test_that("format_cruise_summary_for_prompt works without unclassified_fractions", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_visit1",
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(station_summary)
  expect_type(result, "character")
  expect_false(grepl("UNCLASSIFIED", result))
})

test_that("call_openai errors without API key", {
  withr::with_envvar(c(OPENAI_API_KEY = ""), {
    expect_error(
      algaware:::call_openai("system", "user"),
      "OPENAI_API_KEY"
    )
  })
})

test_that("call_gemini errors without API key", {
  withr::with_envvar(c(GEMINI_API_KEY = ""), {
    expect_error(
      algaware:::call_gemini("system", "user"),
      "GEMINI_API_KEY"
    )
  })
})

test_that("llm_provider returns correct provider", {
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test", GEMINI_API_KEY = ""), {
    expect_equal(llm_provider(), "openai")
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = "AIza-test"), {
    expect_equal(llm_provider(), "gemini")
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = ""), {
    expect_equal(llm_provider(), "none")
  })
})

test_that("llm_provider prefers OpenAI when both keys set", {
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test", GEMINI_API_KEY = "AIza-test"), {
    expect_equal(llm_provider(), "openai")
  })
})

test_that("llm_available detects Gemini key", {
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = "AIza-test"), {
    expect_true(llm_available())
  })
})

test_that("call_llm errors with no provider", {
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = ""), {
    expect_error(
      algaware:::call_llm("system", "user"),
      "No LLM API key configured"
    )
  })
})

test_that("llm_providers returns all available providers", {
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test", GEMINI_API_KEY = "AIza-test"), {
    providers <- llm_providers()
    expect_equal(providers, c("openai", "gemini"))
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = "AIza-test"), {
    expect_equal(llm_providers(), "gemini")
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = ""), {
    expect_equal(llm_providers(), character(0))
  })
})

test_that("llm_model_name returns correct default per provider", {
  withr::with_envvar(c(OPENAI_MODEL = "", GEMINI_MODEL = ""), {
    expect_equal(llm_model_name("openai"), "gpt-5.1")
    expect_equal(llm_model_name("gemini"), "gemini-2.5-flash")
  })
})

test_that("llm_model_name respects env var override", {
  withr::with_envvar(c(OPENAI_MODEL = "gpt-5"), {
    expect_equal(llm_model_name("openai"), "gpt-5")
  })
  withr::with_envvar(c(GEMINI_MODEL = "gemini-3.0-flash"), {
    expect_equal(llm_model_name("gemini"), "gemini-3.0-flash")
  })
})

test_that("llm_model_name returns 'none' for unknown provider", {
  result <- llm_model_name("unknown_provider")
  expect_equal(result, "none")
})

test_that("format_report_paragraph handles text before species name", {
  taxa <- data.frame(
    name = "Skeletonema marinoi",
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "During the cruise, Skeletonema marinoi dominated.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_station_data_for_prompt handles zero total biovolume", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0,
    carbon_ug_per_liter = 0,
    counts_per_liter = 0,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data)
  expect_type(result, "character")
  expect_true(grepl("0%", result))
})

test_that("format_station_data_for_prompt lists HAB species at lower abundance", {
  # Need 16+ taxa so a HAB species at rank 16 is outside the top-15 cut-off
  n_other <- 15
  other_names <- paste0("Taxon_", seq_len(n_other))
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c(other_names, "Alexandrium catenella"),
    biovolume_mm3_per_liter = c(seq(1.0, 0.1, length.out = n_other), 0.001),
    carbon_ug_per_liter = c(seq(10, 1, length.out = n_other), 0.01),
    counts_per_liter = c(seq(1000, 100, length.out = n_other), 5),
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Alexandrium catenella",
    HAB = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data, taxa)
  expect_true(grepl("Other HAB species", result))
  expect_true(grepl("Alexandrium catenella", result))
})

test_that("format_cruise_summary_for_prompt includes chl when present", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_visit1",
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    chl_mean = 2.75,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(station_summary)
  expect_true(grepl("Chl", result))
  expect_true(grepl("2.75", result))
})

test_that("format_station_data_for_prompt flags taxa exceeding warning level", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholm Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Dinophysis acuminata", "Skeletonema marinoi"),
    sflag = c("", ""),
    biovolume_mm3_per_liter = c(0.3, 0.5),
    carbon_ug_per_liter = c(5.0, 8.0),
    counts_per_liter = c(2000, 500),
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = c("Dinophysis acuminata", "Skeletonema marinoi"),
    sflag = c("", ""),
    HAB = c(TRUE, FALSE),
    warning_level = c(1500, NA),
    italic = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data, taxa)
  # Counts (2000) exceed threshold (1500) -> warning should appear
  expect_true(grepl("WARNING", result))
  expect_true(grepl("1500", result))
  expect_true(grepl("IMPORTANT", result))
})

test_that("format_station_data_for_prompt does not flag taxa below warning level", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholm Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Dinophysis acuminata",
    sflag = "",
    biovolume_mm3_per_liter = 0.3,
    carbon_ug_per_liter = 5.0,
    counts_per_liter = 500,
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Dinophysis acuminata",
    sflag = "",
    HAB = TRUE,
    warning_level = 1500,
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data, taxa)
  # Counts (500) below threshold (1500) -> no warning
  expect_false(grepl("WARNING", result))
  expect_false(grepl("IMPORTANT", result))
})

test_that("format_cruise_summary_for_prompt flags warning exceedances", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_v1",
    name = "Dinophysis acuminata",
    sflag = "",
    biovolume_mm3_per_liter = 0.3,
    carbon_ug_per_liter = 5.0,
    counts_per_liter = 2000,
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Dinophysis acuminata",
    sflag = "",
    HAB = TRUE,
    warning_level = 1500,
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(station_summary, taxa)
  expect_true(grepl("WARNING EXCEEDED", result))
  expect_true(grepl("IMPORTANT", result))
  expect_true(grepl("1500", result))
})

# ── bloom_alert_note ────────────────────────────────────────────────────────

# bloom_call() builds a minimal station_summary + matching phyto_groups and
# calls bloom_alert_note() in one step. The `groups` argument is a named
# numeric vector of biovolume where names are text_group values recognised by
# attach_text_groups() (e.g. "Diatoms", "Cyanobacteria", "Other").
bloom_call <- function(coast, month, groups, chl = NA_real_, lang = "en") {
  date <- as.Date(sprintf("2024-%02d-15", month))
  ss <- data.frame(
    STATION_NAME_SHORT      = "TST",
    COAST                   = coast,
    visit_date              = date,
    visit_id                = "TST_v1",
    name                    = names(groups),
    AphiaID                 = seq_along(groups),
    biovolume_mm3_per_liter = unname(groups),
    carbon_ug_per_liter     = unname(groups) * 10,
    counts_per_liter        = unname(groups) * 1000,
    chl_mean                = chl,
    stringsAsFactors        = FALSE
  )
  # phyto_groups maps each synthetic species name to itself as the group so
  # that attach_text_groups() assigns the expected text_group label.
  pg <- data.frame(
    name                       = names(groups),
    AphiaID                    = seq_along(groups),
    phyto_group.plankton_group = names(groups),
    stringsAsFactors           = FALSE
  )
  algaware:::bloom_alert_note(ss, pg, lang = lang)
}

test_that("bloom_alert_note returns empty string when no conditions met", {
  expect_equal(bloom_call("EAST", 3L, c(Diatoms = 0.5, Other = 0.5), chl = 1.0), "")
})

test_that("bloom_alert_note returns empty string for NULL input", {
  expect_equal(algaware:::bloom_alert_note(NULL), "")
})

test_that("bloom_alert_note returns empty string when no date column", {
  ss <- data.frame(COAST = "WEST", biovolume_mm3_per_liter = 1,
                   stringsAsFactors = FALSE)
  expect_equal(algaware:::bloom_alert_note(ss), "")
})

test_that("bloom_alert_note detects spring bloom on West Coast in January", {
  result <- bloom_call("WEST", 1L, c(Diatoms = 0.8, Other = 0.2), chl = 4.5)
  expect_true(nzchar(result))
  expect_true(grepl("SPRING BLOOM", result))
  expect_false(grepl("CYANOBACTERIAL", result))
})

test_that("bloom_alert_note detects spring bloom in February", {
  result <- bloom_call("WEST", 2L, c(Diatoms = 0.7, Other = 0.3), chl = 3.5)
  expect_true(grepl("SPRING BLOOM", result))
})

test_that("bloom_alert_note spring bloom: no alert when chl <= 3", {
  expect_equal(bloom_call("WEST", 1L, c(Diatoms = 0.8, Other = 0.2), chl = 2.9), "")
})

test_that("bloom_alert_note spring bloom: no alert when diatoms <= 50%", {
  expect_equal(bloom_call("WEST", 1L, c(Diatoms = 0.5, Other = 0.5), chl = 5.0), "")
})

test_that("bloom_alert_note spring bloom: no alert outside Jan-Feb", {
  expect_equal(bloom_call("WEST", 3L, c(Diatoms = 0.9, Other = 0.1), chl = 5.0), "")
})

test_that("bloom_alert_note spring bloom: no alert for Baltic stations", {
  expect_equal(bloom_call("EAST", 1L, c(Diatoms = 0.9, Other = 0.1), chl = 5.0), "")
})

test_that("bloom_alert_note detects cyanobacterial bloom in Baltic in July", {
  result <- bloom_call("EAST", 7L, c(Cyanobacteria = 0.6, Other = 0.4), chl = 4.0)
  expect_true(nzchar(result))
  expect_true(grepl("CYANOBACTERIAL BLOOM", result))
  expect_false(grepl("SPRING BLOOM", result))
})

test_that("bloom_alert_note detects cyanobacterial bloom in June and August", {
  for (m in c(6L, 8L)) {
    result <- bloom_call("EAST", m, c(Cyanobacteria = 0.6, Other = 0.4), chl = 3.5)
    expect_true(grepl("CYANOBACTERIAL", result), info = paste("month =", m))
  }
})

test_that("bloom_alert_note cyano bloom: no alert when chl <= 3", {
  expect_equal(bloom_call("EAST", 7L, c(Cyanobacteria = 0.7, Other = 0.3), chl = 2.5), "")
})

test_that("bloom_alert_note cyano bloom: no alert when cyano <= 50%", {
  expect_equal(bloom_call("EAST", 7L, c(Cyanobacteria = 0.5, Other = 0.5), chl = 5.0), "")
})

test_that("bloom_alert_note cyano bloom: no alert outside Jun-Aug", {
  expect_equal(bloom_call("EAST", 5L, c(Cyanobacteria = 0.8, Other = 0.2), chl = 5.0), "")
})

test_that("bloom_alert_note cyano bloom: no alert for West Coast stations", {
  expect_equal(bloom_call("WEST", 7L, c(Cyanobacteria = 0.8, Other = 0.2), chl = 5.0), "")
})

test_that("bloom_alert_note Swedish spring bloom uses Swedish text", {
  result <- bloom_call("WEST", 1L, c(Diatoms = 0.8, Other = 0.2), chl = 4.0, lang = "sv")
  expect_true(grepl("V\u00c5RBLOMNING", result))
  expect_false(grepl("SPRING BLOOM", result))
})

test_that("bloom_alert_note Swedish cyano bloom uses Swedish text", {
  result <- bloom_call("EAST", 7L, c(Cyanobacteria = 0.6, Other = 0.4), chl = 4.0, lang = "sv")
  expect_true(grepl("CYANOBAKTERIABLOMNING", result))
  expect_false(grepl("CYANOBACTERIAL BLOOM", result))
})

test_that("bloom_alert_note chl NA is treated as no high chl", {
  expect_equal(bloom_call("WEST", 1L, c(Diatoms = 0.8, Other = 0.2), chl = NA_real_), "")
})

test_that("bloom_alert_note spring bloom fires when any one station exceeds 50%", {
  # Two West Coast stations: one diatom-dominated (80%), one not (10%)
  ss <- data.frame(
    STATION_NAME_SHORT = c("A", "A", "B", "B"),
    COAST      = "WEST",
    visit_date = as.Date("2024-01-15"),
    visit_id   = c("A_v1", "A_v1", "B_v1", "B_v1"),
    name       = c("Diatoms", "Other", "Diatoms", "Other"),
    AphiaID    = 1:4,
    biovolume_mm3_per_liter = c(0.8, 0.2, 0.1, 0.9),
    carbon_ug_per_liter     = c(8, 2, 1, 9),
    counts_per_liter        = c(800, 200, 100, 900),
    chl_mean   = 4.0,
    stringsAsFactors = FALSE
  )
  pg <- data.frame(name = c("Diatoms", "Other"), AphiaID = 1:2,
                   phyto_group.plankton_group = c("Diatoms", "Other"),
                   stringsAsFactors = FALSE)
  result <- algaware:::bloom_alert_note(ss, pg, lang = "en")
  expect_true(grepl("SPRING BLOOM", result))
})

test_that("bloom_alert_note uses median_time when visit_date absent", {
  # Build synthetic data via bloom_call internals but rename the date column
  date <- as.Date("2024-01-15")
  ss <- data.frame(
    STATION_NAME_SHORT = "TST", COAST = "WEST",
    visit_date = date, visit_id = "TST_v1",
    name = c("Diatoms", "Other"), AphiaID = 1:2,
    biovolume_mm3_per_liter = c(0.8, 0.2),
    carbon_ug_per_liter = c(8, 2), counts_per_liter = c(800, 200),
    chl_mean = 4.0, stringsAsFactors = FALSE
  )
  pg <- data.frame(name = c("Diatoms", "Other"), AphiaID = 1:2,
                   phyto_group.plankton_group = c("Diatoms", "Other"),
                   stringsAsFactors = FALSE)
  names(ss)[names(ss) == "visit_date"] <- "median_time"
  result <- algaware:::bloom_alert_note(ss, pg, lang = "en")
  expect_true(grepl("SPRING BLOOM", result))
})
