# Auto-save the corrections log to the local storage path

Writes the same enriched corrections CSV that the "Download corrections"
button produces (see
[`enrich_corrections_for_export()`](https://nodc-sweden.github.io/ifcb-algaware/reference/enrich_corrections_for_export.md))
to `<storage_path>/corrections/algaware_corrections_<YYYYMMDD>.csv`, so
that work lost to a crash – or to closing the app without downloading –
can be recovered with the existing "Import corrections" button. The
corrections log is cumulative, so each save overwrites the day's file
with the complete state.

## Usage

``` r
autosave_corrections(
  corrections,
  custom_classes,
  storage_path,
  backup_existing = FALSE
)
```

## Arguments

- corrections:

  Data frame of corrections (from `rv$corrections`).

- custom_classes:

  Data frame of custom classes (from `rv$custom_classes`).

- storage_path:

  The local storage path (from settings).

- backup_existing:

  Set `TRUE` on a session's first save: an already-existing target file
  must then come from an earlier session (e.g. one that crashed), so it
  is set aside as `..._prev.csv` instead of being overwritten. Later
  saves in the same session overwrite in place.

## Value

A list with `success` (logical), `path` (the file written, or the
intended target on failure) and `error` (message, or NULL). Returns
`success = FALSE` without writing when there is nothing to save or no
storage path is configured.

## Details

The CSV is first written to a temporary file in the same folder and then
renamed into place, so a crash mid-write can never leave a truncated
file where a previous good autosave was.
