# Escape text for use inside a ggtext markdown label

Replaces `&`, `<` and `>` with their HTML entities so an arbitrary taxon
name can be embedded in
[`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
labels without being parsed as markup.

## Usage

``` r
escape_markdown_label(x)
```

## Arguments

- x:

  Character vector.

## Value

Character vector of the same length, escaped.
