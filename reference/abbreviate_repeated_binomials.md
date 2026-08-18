# Abbreviate repeated species binomials across a running text sequence

Standard biological convention writes a species binomial in full at its
first mention (*Nodularia spumigena*) and abbreviates the genus
thereafter (*N. spumigena*). The LLM applies this within a single
generated chunk, but the station descriptions are generated separately,
so each one re-expands names. This function carries the convention
across the whole station-reports section: given the set of binomials
already written out in full earlier in the section, it abbreviates every
repeat in the current text. The first mention of a binomial anywhere in
the section is left in full; all later mentions, in this or a later
text, are abbreviated.

## Usage

``` r
abbreviate_repeated_binomials(text, taxa_lookup, seen = character(0))
```

## Arguments

- text:

  Character string (one station description).

- taxa_lookup:

  Data frame with a `name` column and optionally an `italic` column.

- seen:

  Character vector of binomials already written out in full earlier in
  the section.

## Value

A list with `text` (the rewritten string) and `seen` (the updated vector
of binomials seen in full).

## Details

Only two-word italic binomials from `taxa_lookup` are considered;
genus-only and "Genus spp." forms are left untouched. A trailing HAB
asterisk (and any following text) is preserved. Genus initials are
abbreviated even when two genera share the same letter, matching the
standard convention and the existing within-paragraph behaviour.
