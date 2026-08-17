#' Ensure all HAB taxa in text are followed by an asterisk
#'
#' Post-processes LLM-generated text to add a trailing \code{*} after any HAB
#' taxon name that is missing one. This makes HAB marking deterministic
#' regardless of whether the LLM remembered to include the asterisk.
#'
#' Genus names (single word, e.g. \emph{Pseudochattonella}) also match their
#' \emph{spp.} / \emph{sp.} form so the asterisk lands after the full mention
#' rather than mid-name.  Abbreviated species names (e.g. \emph{D. acuminata})
#' are matched alongside the full form.
#'
#' @param text Character string of plain text.
#' @param taxa_lookup Data frame with columns \code{name} and \code{HAB}.
#' @return Character string with asterisks added after every HAB taxon mention.
#' @keywords internal
ensure_hab_asterisks <- function(text, taxa_lookup) {
  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0) return(text)
  if (!"HAB" %in% names(taxa_lookup)) return(text)

  # which() drops NA comparisons: custom classes can carry HAB = NA or an NA
  # name, and NA subscripts would inject an NA row whose name becomes the
  # literal pattern "NA", asterisking the word "NA" anywhere in the text.
  hab_rows <- taxa_lookup[which(taxa_lookup$HAB == TRUE &
                                  !is.na(taxa_lookup$name) &
                                  nzchar(taxa_lookup$name)), ]
  if (nrow(hab_rows) == 0) return(text)

  hab_names <- unique(hab_rows$name)

  # Generate abbreviated forms for two-word species names (e.g. "D. acuminata")
  species_names <- hab_names[grepl(" ", hab_names)]
  abbreviations <- vapply(species_names, function(sp) {
    parts <- strsplit(sp, " ", fixed = TRUE)[[1]]
    paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
  }, character(1))

  all_names <- unique(c(hab_names, abbreviations))
  # Match longest names first to prevent partial-name substitutions
  all_names <- all_names[order(-nchar(all_names))]

  for (nm in all_names) {
    escaped <- gsub("([.\\\\|()\\[\\]\\{\\}^$+?])", "\\\\\\1", nm)
    if (!grepl(" ", nm)) {
      # Single-word genus names need three passes to avoid regex backtracking
      # that would otherwise cause double asterisks like "Genus* spp.*".
      #
      # Pass 1: Remove a stray asterisk placed between the genus and spp. by
      #         the LLM ("Genus* spp.*" -> "Genus spp.*").
      text <- gsub(paste0("(", escaped, ")\\*(\\s+spp?\\.)"),
                   "\\1\\2", text, perl = TRUE)
      # Pass 2: Add * after the full "Genus spp." / "Genus sp." form if absent.
      text <- gsub(paste0("(", escaped, "\\s+spp?\\.)(?!\\*)"),
                   "\\1*", text, perl = TRUE)
      # Pass 3: Add * after a bare genus that is not followed by spp. / sp.
      #         and not already marked. Also skip a genus followed by any
      #         lowercase word: that is a full binomial ("Dinophysis
      #         acuminata"), and the asterisk would land in the middle of
      #         the species name ("Dinophysis* acuminata").
      text <- gsub(paste0("(", escaped, ")(?!\\s+spp?\\.)(?!\\s+[a-z])(?!\\*)"),
                   "\\1*", text, perl = TRUE)
    } else {
      pattern <- paste0("(", escaped, ")(?!\\*)")
      text <- gsub(pattern, "\\1*", text, perl = TRUE)
    }
  }

  text
}

#' Abbreviate repeated species binomials across a running text sequence
#'
#' Standard biological convention writes a species binomial in full at its
#' first mention (\emph{Nodularia spumigena}) and abbreviates the genus
#' thereafter (\emph{N. spumigena}). The LLM applies this within a single
#' generated chunk, but the station descriptions are generated separately, so
#' each one re-expands names. This function carries the convention across the
#' whole station-reports section: given the set of binomials already written
#' out in full earlier in the section, it abbreviates every repeat in the
#' current text. The first mention of a binomial anywhere in the section is
#' left in full; all later mentions, in this or a later text, are abbreviated.
#'
#' Only two-word italic binomials from \code{taxa_lookup} are considered;
#' genus-only and "Genus spp." forms are left untouched. A trailing HAB
#' asterisk (and any following text) is preserved. Genus initials are
#' abbreviated even when two genera share the same letter, matching the
#' standard convention and the existing within-paragraph behaviour.
#'
#' @param text Character string (one station description).
#' @param taxa_lookup Data frame with a \code{name} column and optionally an
#'   \code{italic} column.
#' @param seen Character vector of binomials already written out in full
#'   earlier in the section.
#' @return A list with \code{text} (the rewritten string) and \code{seen} (the
#'   updated vector of binomials seen in full).
#' @keywords internal
abbreviate_repeated_binomials <- function(text, taxa_lookup,
                                          seen = character(0)) {
  if (is.null(taxa_lookup) || nrow(taxa_lookup) == 0 ||
      length(text) != 1 || is.na(text) || !nzchar(text)) {
    return(list(text = text, seen = seen))
  }

  italic <- if ("italic" %in% names(taxa_lookup)) {
    taxa_lookup$italic == TRUE & !is.na(taxa_lookup$italic)
  } else {
    rep(TRUE, nrow(taxa_lookup))
  }
  names_all <- taxa_lookup$name[italic & !is.na(taxa_lookup$name) &
                                  nzchar(taxa_lookup$name)]
  binomials <- unique(names_all[grepl(" ", names_all, fixed = TRUE)])
  if (length(binomials) == 0) return(list(text = text, seen = seen))

  # Match longer names first so e.g. "Genus species var. x" is handled before
  # the shorter "Genus species".
  binomials <- binomials[order(-nchar(binomials))]

  for (full in binomials) {
    parts <- strsplit(full, " ", fixed = TRUE)[[1]]
    abbr <- paste0(substr(parts[1], 1, 1), ". ",
                   paste(parts[-1], collapse = " "))

    escaped <- gsub("([.\\\\|()\\[\\]\\{\\}^$+?*])", "\\\\\\1", full)
    pattern <- paste0("\\b", escaped, "\\b")
    m <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (m[1] == -1) next

    starts <- as.integer(m)
    lens <- attr(m, "match.length")
    already_seen <- full %in% seen

    # Keep the first mention in full the first time the binomial appears in the
    # section; abbreviate every later occurrence.
    repl <- if (already_seen) seq_along(starts) else seq_along(starts)[-1]
    if (!already_seen) seen <- c(seen, full)
    if (length(repl) == 0) next

    # Replace from last to first so earlier match positions stay valid.
    for (k in rev(repl)) {
      s <- starts[k]
      e <- starts[k] + lens[k] - 1L
      text <- paste0(substring(text, 1, s - 1L), abbr,
                     substring(text, e + 1L))
    }
  }

  list(text = text, seen = seen)
}

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

  # Build list of names to match: full names + abbreviated forms.
  # which() drops NA comparisons (custom classes can carry italic = NA),
  # which would otherwise inject NA into the names and italicise the literal
  # word "NA" throughout the report text.
  italic_names <- taxa_lookup$name[which(taxa_lookup$italic == TRUE &
                                           !is.na(taxa_lookup$name) &
                                           nzchar(taxa_lookup$name))]
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

  # Post-process: any bare asterisk remaining in "normal" pieces is a HAB
  # marker the LLM placed without attaching it to a recognised species name.
  # Extract each * and reclassify it as "hab" so all asterisks are red.
  pieces <- unlist(lapply(pieces, function(p) {
    if (p$type != "normal" || !grepl("\\*", p$text)) return(list(p))
    result <- list()
    remaining <- p$text
    while (nzchar(remaining)) {
      ast <- regexpr("\\*", remaining)
      if (ast == -1) {
        result <- c(result, list(list(text = remaining, type = "normal")))
        break
      }
      if (ast > 1) {
        result <- c(result, list(
          list(text = substr(remaining, 1, ast - 1), type = "normal")
        ))
      }
      result <- c(result, list(list(text = "*", type = "hab")))
      remaining <- substr(remaining, ast + 1L, nchar(remaining))
    }
    result
  }), recursive = FALSE)

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
#' formatting when taxa_lookup is provided. An empty paragraph is inserted
#' between consecutive paragraphs so that multi-region summaries (Baltic /
#' West Coast) are visually separated in the Word document.
#'
#' @param doc An \code{rdocx} object.
#' @param text Plain text string.
#' @param taxa_lookup Optional taxa lookup for formatting.
#' @param style Paragraph style name.
#' @return The modified \code{rdocx} object.
#' @keywords internal
add_formatted_par <- function(doc, text, taxa_lookup = NULL,
                              style = "Normal") {
  # Deterministically add * after any HAB taxon the LLM forgot to mark
  if (!is.null(taxa_lookup)) {
    text <- ensure_hab_asterisks(text, taxa_lookup)
  }

  # Split on double newlines to preserve paragraph breaks from LLM output
  paragraphs <- strsplit(text, "\n\\s*\n")[[1]]
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]

  for (i in seq_along(paragraphs)) {
    # Insert blank separator between consecutive paragraphs (e.g. between
    # the Baltic Sea and West Coast sections in the summaries)
    if (i > 1) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }
    if (is.null(taxa_lookup)) {
      doc <- officer::body_add_par(doc, paragraphs[[i]], style = style)
    } else {
      fp <- format_report_paragraph(paragraphs[[i]], taxa_lookup)
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
  defaults <- c(openai = "gpt-5.1", gemini = "gemini-2.5-flash-lite")
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
  # Guide is UTF-8; read it as such so any non-ASCII text is preserved on
  # non-UTF-8 locales before it is passed to the LLM prompt.
  con <- file(guide_path, encoding = "UTF-8")
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "\n")
}

#' Normalize phytoplankton groups for report text
#'
#' Standardises the group column name from either \code{phyto_group} or
#' \code{phyto_group.plankton_group} into \code{detailed_group} and
#' \code{text_group} (identical; all groups from the YAML config are
#' preserved in both columns).
#'
#' @param phyto_groups Data frame with columns \code{name}, \code{AphiaID},
#'   and either \code{phyto_group} or \code{phyto_group.plankton_group}.
#' @return Data frame with columns \code{name}, \code{AphiaID},
#'   \code{detailed_group}, and \code{text_group}.
#' @keywords internal
normalize_phyto_groups_for_text <- function(phyto_groups) {
  if (is.null(phyto_groups) || nrow(phyto_groups) == 0) {
    return(NULL)
  }

  group_col <- if ("phyto_group" %in% names(phyto_groups)) {
    "phyto_group"
  } else if ("phyto_group.plankton_group" %in% names(phyto_groups)) {
    "phyto_group.plankton_group"
  } else {
    return(NULL)
  }

  out <- phyto_groups[, intersect(c("name", "AphiaID"), names(phyto_groups)),
                      drop = FALSE]
  out$detailed_group <- as.character(phyto_groups[[group_col]])
  out$detailed_group[is.na(out$detailed_group)] <- "Other"
  out$text_group <- out$detailed_group
  unique(out)
}
