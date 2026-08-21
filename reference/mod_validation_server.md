# Validation Module Server

Provides five validation actions:

1.  **Store Annotations**: save selected images to the SQLite database
    (persistent, shared with ClassiPyR)

2.  **Relabel Selected**: move selected images to a different class
    (session-only, logged in rv\$corrections)

3.  **Unclassify Selected**: move selected images to "unclassified" in
    one click (session-only); a fixed-target shortcut for Relabel
    Selected

4.  **Relabel Class**: move ALL images of the current class in the
    current region to a different class (session-only)

5.  **Unclassify Class**: mark an entire class as non-biological /
    unclassified (session-only)

## Usage

``` r
mod_validation_server(id, rv, config)
```

## Arguments

- id:

  Module namespace ID.

- rv:

  Reactive values for app state.

- config:

  Reactive values with settings.

## Value

NULL (side effects only).

## Details

As crash protection, the corrections log is also auto-saved to
`<local_storage_path>/corrections/` (same CSV format as "Download
corrections") whenever the user navigates to another class or region and
on clean session end; see
[`autosave_corrections()`](https://nodc-sweden.github.io/ifcb-algaware/reference/autosave_corrections.md).
