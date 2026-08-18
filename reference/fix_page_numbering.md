# Post-process the report .docx (page numbering and field flags)

Performs two fixes on the assembled document:

1.  Adds `pgNumType` with `start="1"` to the second section (the first
    content section) so page numbers restart after the front page.

2.  Strips `w:dirty="true"` from the page-number field that officer
    emits via `run_word_field()`. The dirty flag makes Word prompt "This
    document contains fields that may refer to other files. Do you want
    to update the fields?" on every open. Removing it suppresses the
    prompt; `PAGE` fields are still recomputed automatically during
    layout, so page numbers display correctly.

## Usage

``` r
fix_page_numbering(docx_path)
```

## Arguments

- docx_path:

  Path to the .docx file.
