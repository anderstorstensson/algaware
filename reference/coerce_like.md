# Coerce a column to the class of a template column

`type_convert()` on a small increment can infer a different type than on
the full archive (e.g. an all-NA column becomes logical). Coerce towards
the cached (template) column so the rows can be bound.

## Usage

``` r
coerce_like(x, template)
```

## Arguments

- x:

  Column from the increment.

- template:

  Column from the cached metadata.

## Value

`x` coerced to `class(template)`.
