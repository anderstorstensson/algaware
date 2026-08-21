# Drop the unclassified class from data destined for LLM text

The "unclassified" classifier category is kept in figures and tables,
but it is not a taxon and readers of the narrative text are interested
in real taxa only. Without this filter it regularly ranked among the top
taxa by biovolume and was narrated as if it were one.

## Usage

``` r
drop_unclassified_for_text(x)
```

## Arguments

- x:

  Data frame with a `name` column (and optionally `class`).

## Value

`x` without rows whose `name`/`class` is "unclassified"
(case-insensitive).
