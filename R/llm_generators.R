#' Detect bloom conditions and return language-specific prompt instructions
#'
#' Checks whether cruise data meets the criteria for a spring bloom (West Coast,
#' Jan-Feb: diatom-dominated, chl > 3 ug/L) or a cyanobacterial bloom (Baltic,
#' Jun-Aug: cyanobacteria-dominated, chl > 3 ug/L) and returns a block of extra
#' instructions to append to the LLM prompt when either condition is met.
#'
#' @param station_summary Full station_summary data frame.
#' @param phyto_groups Optional phytoplankton group table.
#' @param lang \code{"en"} (English) or \code{"sv"} (Swedish).
#' @return Character string with bloom alert instructions, or \code{""}.
#' @keywords internal
bloom_alert_note <- function(station_summary, phyto_groups = NULL,
                             lang = "en") {
  if (is.null(station_summary) || nrow(station_summary) == 0) return("")

  # Extract cruise month from the earliest visit date available
  date_col <- if ("visit_date" %in% names(station_summary)) "visit_date"
              else if ("median_time" %in% names(station_summary)) "median_time"
              else NULL
  if (is.null(date_col)) return("")
  cruise_month <- as.integer(format(as.Date(station_summary[[date_col]][1]), "%m"))

  # Attach group labels (text_group: Diatoms / Dinoflagellates / Cyanobacteria / Other)
  ss <- attach_text_groups(station_summary, phyto_groups)

  alerts <- character(0)

  # Helper: TRUE if diatoms/cyano exceed `threshold` fraction of biovolume at
  # ANY single station (visit_id) in the supplied subset.
  any_station_dominant <- function(df, group, threshold = 0.5) {
    if (nrow(df) == 0 || !"visit_id" %in% names(df)) return(FALSE)
    any(vapply(unique(df$visit_id), function(vid) {
      rows <- df[df$visit_id == vid, , drop = FALSE]
      total <- sum(rows$biovolume_mm3_per_liter, na.rm = TRUE)
      if (total <= 0) return(FALSE)
      grp <- sum(rows$biovolume_mm3_per_liter[rows$text_group == group],
                 na.rm = TRUE)
      (grp / total) > threshold
    }, logical(1L)))
  }

  # --- Spring bloom: West Coast, January-February ---
  if (cruise_month %in% 1:2) {
    wc <- ss[!is.na(ss$COAST) & ss$COAST != "EAST", , drop = FALSE]
    if (nrow(wc) > 0) {
      high_chl       <- any(!is.na(wc$chl_mean) & wc$chl_mean > 3)
      diatom_dominant <- any_station_dominant(wc, "Diatoms", 0.5)
      if (high_chl && diatom_dominant) alerts <- c(alerts, "spring_bloom")
    }
  }

  # --- Cyanobacterial bloom: Baltic Sea, June-August ---
  if (cruise_month %in% 6:8) {
    bal <- ss[!is.na(ss$COAST) & ss$COAST == "EAST", , drop = FALSE]
    if (nrow(bal) > 0) {
      high_chl      <- any(!is.na(bal$chl_mean) & bal$chl_mean > 3)
      cyano_dominant <- any_station_dominant(bal, "Cyanobacteria", 0.5)
      if (high_chl && cyano_dominant) alerts <- c(alerts, "cyano_bloom")
    }
  }

  if (length(alerts) == 0) return("")

  notes <- character(0)
  if ("spring_bloom" %in% alerts) {
    notes <- c(notes, if (lang == "sv") {
      paste0("DATA INDIKERAR V\u00c5RBLOMNING p\u00e5 v\u00e4stkusten (kiselalger dominerar, ",
             "klorofyll > 3 \u00b5g/L). Framh\u00e4v explicit att en v\u00e5rblomning ",
             "p\u00e5g\u00e5r i stycket om v\u00e4stkusten.")
    } else {
      paste0("DATA INDICATES A SPRING BLOOM on the West Coast (diatoms dominant, ",
             "chlorophyll > 3 \u00b5g/L). Explicitly highlight that a spring bloom ",
             "is occurring in the West Coast paragraph.")
    })
  }
  if ("cyano_bloom" %in% alerts) {
    notes <- c(notes, if (lang == "sv") {
      paste0("DATA INDIKERAR CYANOBAKTERIABLOMNING i \u00d6stersj\u00f6n (cyanobakterier dominerar, ",
             "klorofyll > 3 \u00b5g/L). Framh\u00e4v explicit att en cyanobakteriablomning ",
             "p\u00e5g\u00e5r i stycket om \u00d6stersj\u00f6n.")
    } else {
      paste0("DATA INDICATES A CYANOBACTERIAL BLOOM in the Baltic Sea (cyanobacteria dominant, ",
             "chlorophyll > 3 \u00b5g/L). Explicitly highlight that a cyanobacterial bloom ",
             "is occurring in the Baltic Sea paragraph.")
    })
  }

  paste0("\n\nBLOOM ALERTS -- MUST BE MENTIONED:\n",
         paste(notes, collapse = "\n"))
}

#' Generate the Swedish summary text
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup table.
#' @param cruise_info Cruise info string.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @param unclassified_fractions Optional per-sample fractions of unclassified detections supplied to the prompt.
#' @return Character string with Swedish summary.
#' @export
generate_swedish_summary <- function(station_summary, taxa_lookup = NULL,
                                     cruise_info = "", phyto_groups = NULL,
                                     provider = NULL,
                                     unclassified_fractions = NULL) {
  guide <- load_writing_guide()
  cruise_data <- format_cruise_summary_for_prompt(station_summary, taxa_lookup,
                                                  unclassified_fractions = unclassified_fractions,
                                                  phyto_groups = phyto_groups)
  bloom_note <- bloom_alert_note(station_summary, phyto_groups, lang = "sv")

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
    "'kiselalger' (not 'diatom\u00e9er' or 'diatomeer') for diatoms, ",
    "'v\u00e4xtplankton' (not 'fytoplankton'), ",
    "'expedition' (not 'kryssningen'), ",
    "'kryptomonader' (not 'kryptofyter'). ",
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself. ",
    "Om ett potentiellt skadligt taxon dominerar biovolymen vid en station, ",
    "beskriv det som dominant \u2014 tona inte ned dess f\u00f6rekomst enbart p\u00e5 grund av att det \u00e4r potentiellt skadligt. ",
    "Compare klorofyllfluorescens between stations and relate it to the ",
    "IFCB biovolume data where relevant. ",
    "If any station data lines contain 'WARNING EXCEEDED', you MUST explicitly ",
    "state in the summary that the abundance of the named taxon exceeds the ",
    "recommended warning level (varningsniv\u00e5) at that station, and include ",
    "the actual abundance in cells/L and the threshold value. ",
    "Output ONLY the summary text, no headings. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the FULL taxon mention. ",
    "Place it after 'spp.' or 'sp.' when present (e.g. 'Pseudochattonella spp.*', ",
    "NOT 'Pseudochattonella* spp.*'). Never place an asterisk after the genus name alone ",
    "when 'spp.' or 'sp.' follows.",
    bloom_note
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Translate an English summary to Swedish
#'
#' @param english_text Character string with the English summary to translate.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @return Character string with Swedish translation.
#' @export
translate_summary_to_swedish <- function(english_text, provider = NULL) {
  system_prompt <- paste0(
    "You are a translator specializing in marine biology reports for the Swedish ",
    "AlgAware programme (SMHI). Translate scientific phytoplankton monitoring text ",
    "from English to Swedish accurately and completely."
  )

  user_prompt <- paste0(
    "Translate the following English phytoplankton monitoring summary to Swedish. ",
    "Rules:\n",
    "- Translate every sentence; do not omit or condense any content.\n",
    "- Use correct Swedish scientific terminology: ",
    "'klorofyllfluorescens' (not 'chlorophyll fluorescence'), ",
    "'biovolym' (not 'biovolume'), ",
    "'kiselalger' (not 'diatom\u00e9er' or 'diatomeer'), ",
    "'v\u00e4xtplankton' (not 'fytoplankton'), ",
    "'expedition' (not 'kryssningen'), ",
    "'kryptomonader' (not 'kryptofyter'), ",
    "'klorofyll' (not 'chlorophyll').\n",
    "- Keep all species names in Latin (italics not needed in plain text).\n",
    "- Keep the asterisk (*) immediately after any species name that has one ",
    "(it marks potentially harmful taxa).\n",
    "- Output plain text only -- no markdown, no headings, no extra commentary.\n\n",
    "English text to translate:\n\n",
    english_text
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Generate the English summary text
#'
#' @param station_summary Full station_summary data frame.
#' @param taxa_lookup Optional taxa lookup table.
#' @param cruise_info Cruise info string.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @param unclassified_fractions Optional per-sample unclassified percentages
#'   for contextualizing the summary.
#' @return Character string with English summary.
#' @export
generate_english_summary <- function(station_summary, taxa_lookup = NULL,
                                     cruise_info = "", phyto_groups = NULL,
                                     provider = NULL,
                                     unclassified_fractions = NULL) {
  guide <- load_writing_guide()
  cruise_data <- format_cruise_summary_for_prompt(station_summary, taxa_lookup,
                                                  unclassified_fractions = unclassified_fractions,
                                                  phyto_groups = phyto_groups)
  bloom_note <- bloom_alert_note(station_summary, phyto_groups, lang = "en")

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
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself. ",
    "If a potentially harmful taxon dominates by biovolume at a station, name it as ",
    "dominant in the community description \u2014 do not downplay its abundance simply because ",
    "it is potentially harmful. ",
    "If any station data lines contain 'WARNING EXCEEDED', you MUST explicitly ",
    "state in the summary that the abundance of the named taxon exceeds the ",
    "recommended warning level at that station, and include the actual abundance ",
    "in cells/L and the threshold value. ",
    "Output ONLY the summary text, no headings or extra commentary. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the FULL taxon mention. ",
    "Place it after 'spp.' or 'sp.' when present (e.g. 'Pseudochattonella spp.*', ",
    "NOT 'Pseudochattonella* spp.*'). Never place an asterisk after the genus name alone ",
    "when 'spp.' or 'sp.' follows.",
    bloom_note
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}

#' Generate a station description
#'
#' @param station_data Data frame with station_summary rows for one visit.
#' @param taxa_lookup Optional taxa lookup table.
#' @param all_stations_summary Optional full station_summary for context.
#' @param phyto_groups Optional phytoplankton group table used to provide
#'   explicit group assignments in the prompt text.
#' @param provider LLM provider (\code{"openai"} or \code{"gemini"}).
#'   NULL auto-detects.
#' @param unclassified_pct Optional per-class unclassified percentage info used for context.
#' @return Character string with station description in English.
#' @export
generate_station_description <- function(station_data, taxa_lookup = NULL,
                                         all_stations_summary = NULL,
                                         phyto_groups = NULL,
                                         provider = NULL,
                                         unclassified_pct = NULL) {
  guide <- load_writing_guide()
  station_text <- format_station_data_for_prompt(station_data, taxa_lookup,
                                                 unclassified_pct = unclassified_pct,
                                                 phyto_groups = phyto_groups)

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
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself. ",
    "Use the group names consistently throughout: if a taxon belongs to Silicoflagellates, ",
    "call it Silicoflagellates everywhere \u2014 never refer to it as 'Others' in one sentence ",
    "and Silicoflagellates in another. ",
    "Provide a detailed species breakdown only for the top 3 groups listed in the prompt. ",
    "Mention any remaining groups in a single brief sentence. ",
    "Taxa marked [HAB] in the data are potentially harmful and must be mentioned. ",
    "If a [HAB] taxon ranks among the dominant taxa by biovolume, describe it as dominant ",
    "in the community description \u2014 do not omit it from the dominance narrative just because ",
    "it also appears in the potentially harmful taxa section. ",
    "If the station data contains an IMPORTANT section about taxa exceeding warning ",
    "levels, you MUST state in the description the actual abundance in cells/L and ",
    "the threshold value (e.g. '2 000 cells/L, exceeding the warning level of ",
    "1 500 cells/L'). ",
    "Output ONLY the description text, no headings or station name. ",
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the FULL taxon mention. ",
    "Place it after 'spp.' or 'sp.' when present (e.g. 'Pseudochattonella spp.*', ",
    "NOT 'Pseudochattonella* spp.*'). Never place an asterisk after the genus name alone ",
    "when 'spp.' or 'sp.' follows."
  )

  call_llm(system_prompt, user_prompt, provider = provider)
}
