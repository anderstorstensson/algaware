# Sanitize error message for user display

Strips the leading "Error in : " prefix that R prepends to condition
messages so that only the human-readable part is shown in the sidebar.
Only that exact prefix is removed: the old greedy pattern (`"^.*: "`)
stripped everything up to the *last* colon, reducing e.g.
`cannot open URL 'https://...': HTTP status was '404 Not Found'` to just
`'404 Not Found'` – and this is the only error surface the app has.

## Usage

``` r
sanitize_error_msg(msg)
```

## Arguments

- msg:

  Character string (typically `e$message`).

## Value

Simplified character string.
