# Extract and validate the text content of a chat-completions response

A refusal or safety-blocked response carries a NULL `content`; passing
that on silently yielded `character(0)`, which slipped past the callers'
tryCatch fallbacks and crashed report assembly much later with
"subscript out of bounds". Raising here instead lets the existing
placeholder-text fallbacks handle the failure.

## Usage

``` r
extract_llm_content(result)
```

## Arguments

- result:

  Parsed JSON body of a chat-completions response.

## Value

Length-1 character string.
