# Reset per-cruise validation state

Clears the corrections log, user-added custom classes and the gallery
selection when new data is loaded, so corrections made on one cruise are
never carried into – and exported or auto-saved together with – the next
cruise loaded in the same session. Column structure is preserved.

## Usage

``` r
reset_corrections_state(rv)
```

## Arguments

- rv:

  [`shiny::reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  (or a list-like object) holding `corrections`, `custom_classes` and
  `selected_images`.

## Value

`rv`, invisibly, after modification.
