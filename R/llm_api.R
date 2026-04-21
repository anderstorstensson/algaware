# Package-level mutable state for rate limiting (environment, not exported)
.llm_state <- new.env(parent = emptyenv())
.llm_state$gemini_last_call <- NULL

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
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
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
#' @param model Gemini model name (default: "gemini-2.5-flash-lite").
#' @param temperature Sampling temperature (default: 0.3).
#' @return Character string with the generated text.
#' @keywords internal
call_gemini <- function(system_prompt, user_prompt,
                        model = llm_model_name("gemini"),
                        temperature = 0.3) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
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
      max_tries = 5,
      # 429 = rate-limited; 503 = service overload -- both are transient
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L),
      # Exponential backoff: 15 s, 30 s, 60 s, 120 s (capped)
      backoff = \(attempt) min(gemini_delay * 2^(attempt - 1L), 120)
    ) |>
    httr2::req_perform()

  .llm_state$gemini_last_call <- Sys.time()

  result <- httr2::resp_body_json(resp)
  text <- result$choices[[1]]$message$content
  strip_markdown(text)
}

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
