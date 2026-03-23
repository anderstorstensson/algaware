#' LLM Text Generation for AlgAware Reports
#'
#' Functions for generating report text using OpenAI or Google Gemini.
#' Requires either OPENAI_API_KEY or GEMINI_API_KEY to be set.
#'
#' @name llm
NULL

#' Build a formatted paragraph with italic species names and red HAB asterisks
#'
#' Parses plain text and returns an \code{officer::fpar} object with italic
#' formatting for recognized species/genus names and red bold formatting for
#' HAB asterisks.
#'
#' @param text Character string of plain text.
#' @param taxa_lookup Data frame with columns \code{name}, \code{italic},
#'   and optionally \code{HAB}.
#' @return An \code{officer::fpar} object.
#' @keywords internal
format_report_paragraph <- function(text, taxa_lookup = NULL) {
  # Font properties for the Word report. Adobe Garamond Pro is the standard
  # font used in SMHI AlgAware publications. If not installed, Word will
  # substitute a similar serif font.
  normal_prop <- officer::fp_text(font.size = 11, font.family = "Adobe Garamond Pro")
  italic_prop <- officer::fp_text(font.size = 11, italic = TRUE, font.family = "Adobe Garamond Pro")
  red_prop <- officer::fp_text(font.size = 11, color = "red", bold = TRUE, font.family = "Adobe Garamond Pro")

  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0) {
    return(officer::fpar(officer::ftext(text, normal_prop)))
  }

  # Build list of names to match: full names + abbreviated forms
  italic_names <- taxa_lookup$name[taxa_lookup$italic == TRUE &
                                     nzchar(taxa_lookup$name)]
  italic_names <- unique(italic_names)

  # Generate abbreviated forms for species (two-word names)
  species_names <- italic_names[grepl(" ", italic_names)]
  abbreviations <- vapply(species_names, function(sp) {
    parts <- strsplit(sp, " ", fixed = TRUE)[[1]]
    paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
  }, character(1))

  # Combine: match longest first to avoid partial matches
  all_names <- c(italic_names, abbreviations)
  all_names <- unique(all_names[nzchar(all_names)])
  all_names <- all_names[order(-nchar(all_names))]

  # Escape regex special chars in names, allow optional * suffix for HAB
  patterns <- vapply(all_names, function(n) {
    escaped <- gsub("([.\\\\|()\\[\\]\\{\\}^$+?])", "\\\\\\1", n)
    paste0("(", escaped, ")(\\*?)")
  }, character(1))

  combined_pattern <- paste(patterns, collapse = "|")

  # Split text on species name matches
  pieces <- list()
  remaining <- text
  while (nzchar(remaining)) {
    m <- regexpr(combined_pattern, remaining, perl = TRUE)
    if (m == -1) {
      pieces <- c(pieces, list(list(text = remaining, type = "normal")))
      break
    }

    # Text before the match
    if (m > 1) {
      before <- substr(remaining, 1, m - 1)
      pieces <- c(pieces, list(list(text = before, type = "normal")))
    }

    match_len <- attr(m, "match.length")
    matched <- substr(remaining, m, m + match_len - 1)

    # Check if it ends with HAB asterisk
    if (grepl("\\*$", matched)) {
      species_part <- sub("\\*$", "", matched)
      pieces <- c(pieces, list(
        list(text = species_part, type = "italic"),
        list(text = "*", type = "hab")
      ))
    } else {
      pieces <- c(pieces, list(list(text = matched, type = "italic")))
    }

    remaining <- substr(remaining, m + match_len, nchar(remaining))
  }

  # Build fpar from pieces
  ftext_args <- lapply(pieces, function(p) {
    prop <- switch(p$type,
      italic = italic_prop,
      hab = red_prop,
      normal_prop
    )
    officer::ftext(p$text, prop)
  })

  do.call(officer::fpar, ftext_args)
}

#' Add a formatted paragraph to a Word document
#'
#' Wrapper around \code{officer::body_add_fpar} that applies species name
#' formatting when taxa_lookup is provided.
#'
#' @param doc An \code{rdocx} object.
#' @param text Plain text string.
#' @param taxa_lookup Optional taxa lookup for formatting.
#' @param style Paragraph style name.
#' @return The modified \code{rdocx} object.
#' @keywords internal
add_formatted_par <- function(doc, text, taxa_lookup = NULL,
                              style = "Normal") {
  # Split on double newlines to preserve paragraph breaks from LLM output
  paragraphs <- strsplit(text, "\n\\s*\n")[[1]]
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]

  for (para in paragraphs) {
    if (is.null(taxa_lookup)) {
      doc <- officer::body_add_par(doc, para, style = style)
    } else {
      fp <- format_report_paragraph(para, taxa_lookup)
      doc <- officer::body_add_fpar(doc, fp, style = style)
    }
  }
  doc
}

#' Check if LLM text generation is available
#'
#' @return TRUE if OPENAI_API_KEY or GEMINI_API_KEY is set, FALSE otherwise.
#' @export
llm_available <- function() {
  nzchar(Sys.getenv("OPENAI_API_KEY", "")) ||
    nzchar(Sys.getenv("GEMINI_API_KEY", ""))
}

#' List available LLM providers
#'
#' @return Character vector of provider names with valid API keys.
#' @export
llm_providers <- function() {
  providers <- character(0)
  if (nzchar(Sys.getenv("OPENAI_API_KEY", ""))) {
    providers <- c(providers, "openai")
  }
  if (nzchar(Sys.getenv("GEMINI_API_KEY", ""))) {
    providers <- c(providers, "gemini")
  }
  providers
}

#' Detect the default LLM provider
#'
#' @return Character string: \code{"openai"}, \code{"gemini"}, or \code{"none"}.
#'   When both keys are set, OpenAI is preferred.
#' @export
llm_provider <- function() {
  providers <- llm_providers()
  if (length(providers) == 0) return("none")
  providers[1]
}

#' Get the model name for a provider
#'
#' Uses the environment variable \code{OPENAI_MODEL} or \code{GEMINI_MODEL}
#' if set, otherwise falls back to built-in defaults.
#'
#' @param provider Character string: \code{"openai"} or \code{"gemini"}.
#'   Defaults to the active provider.
#' @return Character string with the model name.
#' @export
llm_model_name <- function(provider = llm_provider()) {
  defaults <- c(openai = "gpt-4.1", gemini = "gemini-2.5-flash")
  env_vars <- c(openai = "OPENAI_MODEL", gemini = "GEMINI_MODEL")

  if (!provider %in% names(defaults)) return("none")

  env_val <- Sys.getenv(env_vars[[provider]], "")
  if (nzchar(env_val)) env_val else defaults[[provider]]
}

#' Load the report writing guide
#'
#' @return Character string with the writing guide content.
#' @keywords internal
load_writing_guide <- function() {
  guide_path <- system.file("extdata", "report_writing_guide.md",
                            package = "algaware")
  if (!nzchar(guide_path) || !file.exists(guide_path)) {
    return("")
  }
  paste(readLines(guide_path, warn = FALSE), collapse = "\n")
}

#' Format station data as a text summary for the LLM prompt
#'
#' @param station_data Data frame with station_summary rows for one visit.
#' @param taxa_lookup Optional taxa lookup with HAB column.
#' @return Character string describing the station data.
#' @keywords internal
format_station_data_for_prompt <- function(station_data, taxa_lookup = NULL) {
  station_name <- station_data$STATION_NAME_SHORT[1]
  full_name <- station_data$STATION_NAME[1]
  coast <- station_data$COAST[1]
  visit_date <- as.character(station_data$visit_date[1])
  region <- if (coast == "EAST") "Baltic Sea" else "West Coast (Skagerrak/Kattegat)"

  # Sort by biovolume descending
  station_data <- station_data[order(-station_data$biovolume_mm3_per_liter), ]

  # Total biomass

  total_carbon <- sum(station_data$carbon_ug_per_liter, na.rm = TRUE)
  total_biovolume <- sum(station_data$biovolume_mm3_per_liter, na.rm = TRUE)
  total_counts <- sum(station_data$counts_per_liter, na.rm = TRUE)
  n_taxa <- nrow(station_data)

  # Chlorophyll if available
  chl_info <- ""
  if ("chl_mean" %in% names(station_data) &&
      !all(is.na(station_data$chl_mean))) {
    chl_val <- station_data$chl_mean[1]
    if (!is.na(chl_val)) {
      chl_info <- sprintf("Chlorophyll fluorescence (mean): %.2f", chl_val)
    }
  }

  # Top taxa by biovolume
  top_n <- min(15, nrow(station_data))
  top_taxa <- station_data[seq_len(top_n), ]

  # Identify HAB species
  hab_species <- character(0)
  if (!is.null(taxa_lookup) && "HAB" %in% names(taxa_lookup)) {
    hab_names <- taxa_lookup$name[taxa_lookup$HAB == TRUE]
    hab_species <- intersect(station_data$name, hab_names)
  }

  # Build taxa table
  taxa_lines <- vapply(seq_len(nrow(top_taxa)), function(i) {
    row <- top_taxa[i, ]
    hab_flag <- if (row$name %in% hab_species) " [HAB]" else ""
    pct <- if (total_biovolume > 0) {
      sprintf("%.1f%%", row$biovolume_mm3_per_liter / total_biovolume * 100)
    } else {
      "0%"
    }
    sprintf("  %s%s: %.3f mm3/L (%s of total), %.0f counts/L",
            row$name, hab_flag,
            row$biovolume_mm3_per_liter, pct,
            row$counts_per_liter)
  }, character(1))

  # Additional HAB species in lower abundances
  hab_in_sample <- station_data$name[station_data$name %in% hab_species]
  hab_not_in_top <- setdiff(hab_in_sample, top_taxa$name)
  hab_extra <- ""
  if (length(hab_not_in_top) > 0) {
    hab_extra <- paste0(
      "\nOther HAB species present (lower abundance): ",
      paste(hab_not_in_top, collapse = ", ")
    )
  }

  paste0(
    "Station: ", station_name, " (", full_name, ")\n",
    "Region: ", region, "\n",
    "Date: ", visit_date, "\n",
    "Number of taxa: ", n_taxa, "\n",
    "Total counts per liter: ", sprintf("%.0f", total_counts), "\n",
    "Total biovolume: ", sprintf("%.4f", total_biovolume), " mm3/L\n",
    "Total carbon biomass: ", sprintf("%.2f", total_carbon), " ug/L\n",
    if (nzchar(chl_info)) paste0(chl_info, "\n") else "",
    "\nTop taxa by biovolume:\n",
    paste(taxa_lines, collapse = "\n"),
    hab_extra
  )
}

#' Format cruise-level data summary for LLM prompt
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup with HAB column.
#' @return Character string with cruise-level overview.
#' @keywords internal
format_cruise_summary_for_prompt <- function(station_summary, taxa_lookup = NULL) {
  visits <- unique(station_summary[, c("STATION_NAME_SHORT", "COAST",
                                        "visit_date", "visit_id")])
  visits <- visits[order(visits$COAST, visits$visit_date), ]

  # HAB species lookup
  hab_species <- character(0)
  if (!is.null(taxa_lookup) && "HAB" %in% names(taxa_lookup)) {
    hab_species <- taxa_lookup$name[taxa_lookup$HAB == TRUE]
  }

  # Per-station summaries
  station_lines <- vapply(seq_len(nrow(visits)), function(i) {
    v <- visits[i, ]
    sdata <- station_summary[station_summary$visit_id == v$visit_id, ]
    sdata <- sdata[order(-sdata$biovolume_mm3_per_liter), ]

    total_bv <- sum(sdata$biovolume_mm3_per_liter, na.rm = TRUE)
    total_carbon <- sum(sdata$carbon_ug_per_liter, na.rm = TRUE)
    total_counts <- sum(sdata$counts_per_liter, na.rm = TRUE)
    n_taxa <- nrow(sdata)

    # Top 5 species
    top5 <- head(sdata, 5)
    top_names <- vapply(seq_len(nrow(top5)), function(j) {
      hab_flag <- if (top5$name[j] %in% hab_species) "*" else ""
      paste0(top5$name[j], hab_flag)
    }, character(1))

    # HAB present
    hab_found <- intersect(sdata$name, hab_species)
    hab_note <- if (length(hab_found) > 0) {
      paste0(" | HAB: ", paste(hab_found, collapse = ", "))
    } else {
      ""
    }

    # Chlorophyll
    chl_note <- ""
    if ("chl_mean" %in% names(sdata) && !all(is.na(sdata$chl_mean))) {
      chl_val <- sdata$chl_mean[1]
      if (!is.na(chl_val)) {
        chl_note <- sprintf(" | Chl: %.2f", chl_val)
      }
    }

    region <- if (v$COAST == "EAST") "Baltic" else "West"
    sprintf("  %s (%s, %s): %d taxa, %.0f counts/L, %.4f mm3/L biovol, %.2f ug/L carbon | Top: %s%s%s",
            v$STATION_NAME_SHORT, region, v$visit_date,
            n_taxa, total_counts, total_bv, total_carbon,
            paste(top_names, collapse = ", "),
            hab_note, chl_note)
  }, character(1))

  paste(station_lines, collapse = "\n")
}

#' Call the OpenAI API
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model OpenAI model name (default: "gpt-4.1").
#' @param temperature Sampling temperature (default: 0.3). Lower values
#'   produce more deterministic, factual output; higher values are more
#'   creative. 0.3 is a good balance for scientific report text.
#' @return Character string with the generated text.
#' @keywords internal
call_openai <- function(system_prompt, user_prompt,
                        model = llm_model_name("openai"),
                        temperature = 0.3) {
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY environment variable is not set.", call. = FALSE)
  }

  body <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  # 120s timeout because report text generation can be slow for long prompts.
  # Retries twice with 5s backoff to handle transient API errors.
  resp <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 2, backoff = ~ 5) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)
  text <- result$choices[[1]]$message$content
  strip_markdown(text)
}

#' Call the Gemini API (OpenAI-compatible endpoint)
#'
#' Uses Google's OpenAI-compatible chat completions endpoint. Includes a
#' rate-limit delay to stay within the free tier (5 RPM for Pro, 15 RPM
#' for Flash). On 429 responses, waits and retries up to 3 times with
#' increasing backoff.
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model Gemini model name (default: "gemini-2.5-flash").
#' @param temperature Sampling temperature (default: 0.3).
#' @return Character string with the generated text.
#' @keywords internal
call_gemini <- function(system_prompt, user_prompt,
                        model = llm_model_name("gemini"),
                        temperature = 0.3) {
  api_key <- Sys.getenv("GEMINI_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("GEMINI_API_KEY environment variable is not set.", call. = FALSE)
  }

  body <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  # Rate-limit delay: Gemini free tier allows 5 RPM for 2.5 Pro (one
  # request per 12s). Default 15s provides margin for variable response
  # times. Override with options(algaware.gemini_delay = <seconds>).
  gemini_delay <- getOption("algaware.gemini_delay", 15)
  last_call <- .llm_state$gemini_last_call
  if (!is.null(last_call)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_call, units = "secs"))
    if (elapsed < gemini_delay) {
      Sys.sleep(gemini_delay - elapsed)
    }
  }

  .llm_state$gemini_last_call <- Sys.time()

  resp <- httr2::request("https://generativelanguage.googleapis.com/v1beta/openai/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(
      max_tries = 2,
      is_transient = \(resp) httr2::resp_status(resp) == 429,
      backoff = \(attempt) gemini_delay * attempt
    ) |>
    httr2::req_perform()

  .llm_state$gemini_last_call <- Sys.time()

  result <- httr2::resp_body_json(resp)
  text <- result$choices[[1]]$message$content
  strip_markdown(text)
}

# Package-level mutable state for rate limiting (environment, not exported)
.llm_state <- new.env(parent = emptyenv())
.llm_state$gemini_last_call <- NULL

#' Call an LLM provider
#'
#' Dispatches to \code{call_openai} or \code{call_gemini}. When
#' \code{provider} is NULL, auto-detects from available API keys.
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param provider Character string: \code{"openai"} or \code{"gemini"}.
#'   NULL (default) auto-detects.
#' @param temperature Sampling temperature (default: 0.3).
#' @return Character string with the generated text.
#' @keywords internal
call_llm <- function(system_prompt, user_prompt, provider = NULL,
                     temperature = 0.3) {
  if (is.null(provider)) provider <- llm_provider()
  switch(provider,
    openai = call_openai(system_prompt, user_prompt,
                         temperature = temperature),
    gemini = call_gemini(system_prompt, user_prompt,
                         temperature = temperature),
    stop("No LLM API key configured. Set OPENAI_API_KEY or GEMINI_API_KEY.",
         call. = FALSE)
  )
}

#' Strip markdown formatting from LLM output
#'
#' Removes markdown bold/italic markers while preserving HAB asterisks
#' (asterisk directly after a word with no space).
#'
#' @param text Character string.
#' @return Cleaned character string.
#' @keywords internal
strip_markdown <- function(text) {
  # Remove **bold** markers
  text <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text)
  # Remove *italic* markers (asterisk-word-asterisk with no adjacent letter)
  # But preserve HAB markers like "species_name*" (no closing asterisk)
  text <- gsub("(?<![\\w])\\*([^*]+)\\*(?![\\w])", "\\1", text, perl = TRUE)
  # Remove any leading/trailing whitespace
  trimws(text)
}

#' Generate the Swedish summary text
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup table.
#' @param cruise_info Cruise info string.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @return Character string with Swedish summary.
#' @export
generate_swedish_summary <- function(station_summary, taxa_lookup = NULL,
                                     cruise_info = "", provider = NULL) {
  guide <- load_writing_guide()
  cruise_data <- format_cruise_summary_for_prompt(station_summary, taxa_lookup)

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in Swedish. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write the Swedish summary (Sammanfattning) for this AlgAware cruise report.\n\n",
    "Cruise: ", cruise_info, "\n\n",
    "Station data overview:\n", cruise_data, "\n\n",
    "Write the summary in Swedish, using one paragraph per region ",
    "(West Coast and Baltic Sea), clearly separated. Follow the style described in the ",
    "writing guide. Mark potentially harmful taxa (potentiellt skadliga taxa) ",
    "with an asterisk (*). Never use the term 'HAB species' or 'HAB-arter'; ",
    "say 'potentiellt skadligt taxon' (singular) or 'potentiellt skadliga taxa' ",
    "(plural) instead. Use 'potentiellt skadlig art' only when referring to a ",
    "specific species. ",
    "Use correct Swedish terminology: 'klorofyll' (not 'chlorophyll'), ",
    "'klorofyllfluorescens' (not 'chlorophyll fluorescence'), ",
    "'biovolym' (not 'biovolume'), ",
    "'kiselalger' (not 'diatom\u00e9er' or 'diatomeer') for diatoms. ",
    "Compare klorofyllfluorescens between stations and relate it to the ",
    "IFCB biovolume data where relevant. ",
    "Output ONLY the summary text, no headings. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker directly after a species name."
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Generate the English summary text
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup table.
#' @param cruise_info Cruise info string.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @return Character string with English summary.
#' @export
generate_english_summary <- function(station_summary, taxa_lookup = NULL,
                                     cruise_info = "", provider = NULL) {
  guide <- load_writing_guide()
  cruise_data <- format_cruise_summary_for_prompt(station_summary, taxa_lookup)

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in English. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write the English summary (Abstract) for this AlgAware cruise report.\n\n",
    "Cruise: ", cruise_info, "\n\n",
    "Station data overview:\n", cruise_data, "\n\n",
    "Write in English. Follow the writing guide exactly, including the ",
    "harmful taxa terminology section. ",
    "Output ONLY the summary text, no headings or extra commentary. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker directly after a species name."
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Generate a station description
#'
#' @param station_data Data frame with station_summary rows for one visit.
#' @param taxa_lookup Optional taxa lookup table.
#' @param all_stations_summary Optional full station_summary for context.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @return Character string with station description in English.
#' @export
generate_station_description <- function(station_data, taxa_lookup = NULL,
                                         all_stations_summary = NULL,
                                         provider = NULL) {
  guide <- load_writing_guide()
  station_text <- format_station_data_for_prompt(station_data, taxa_lookup)

  # Provide cruise-wide context so the LLM can make relative statements
  context <- ""
  if (!is.null(all_stations_summary)) {
    all_visits <- unique(all_stations_summary[, c("visit_id",
                                                   "STATION_NAME_SHORT",
                                                   "COAST")])
    context_lines <- vapply(seq_len(nrow(all_visits)), function(i) {
      vid <- all_visits$visit_id[i]
      d <- all_stations_summary[all_stations_summary$visit_id == vid, ]
      n_taxa <- nrow(d)
      total_counts <- sum(d$counts_per_liter, na.rm = TRUE)
      total_bv <- sum(d$biovolume_mm3_per_liter, na.rm = TRUE)
      region <- if (all_visits$COAST[i] == "EAST") "Baltic" else "West"
      current <- if (vid == unique(station_data$visit_id)[1]) " <-- this station" else ""
      sprintf("  %s (%s): %d taxa, %.0f counts/L, %.4f mm3/L biovolume%s",
              all_visits$STATION_NAME_SHORT[i], region,
              n_taxa, total_counts, total_bv, current)
    }, character(1))
    context <- paste0(
      "\n\nCruise context -- compare this station relative to the others:\n",
      paste(context_lines, collapse = "\n")
    )
  }

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in English. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write a station description for the following station visit.\n\n",
    station_text,
    context,
    "\n\nWrite in English. Follow the station description pattern and ",
    "harmful taxa terminology in the writing guide exactly. ",
    "Base your characterization of diversity (high/moderate/low) and abundance ",
    "(high/moderate/low) on the cruise context data above -- describe this station ",
    "relative to the other stations visited during this cruise. ",
    "Taxa marked [HAB] in the data are potentially harmful and must be mentioned. ",
    "Output ONLY the description text, no headings or station name. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker directly after a species name."
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}
