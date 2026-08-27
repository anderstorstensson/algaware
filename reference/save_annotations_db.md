# Save selected annotations to SQLite

Stores annotations for selected images. Compatible with ClassiPyR's
annotation format.

## Usage

``` r
save_annotations_db(
  db_path,
  annotations,
  annotator = "",
  class_list = character(0),
  backfill_rois = NULL
)
```

## Arguments

- db_path:

  Path to the SQLite database file.

- annotations:

  A data.frame with columns: `sample_name`, `roi_number`, `class_name`.

- annotator:

  Name of the annotator.

- class_list:

  Character vector of all class names (for class_lists table).

- backfill_rois:

  Optional data.frame with columns `sample_name` and `roi_number`
  listing the complete ROI set of the affected samples (it may include
  the annotated ROIs; existing rows are skipped).

## Value

Logical TRUE on success, FALSE on failure.

## Details

When `backfill_rois` is supplied, every ROI in it that has no annotation
row yet is additionally inserted as `"unclassified"` with
`is_manual = 0` ("not yet reviewed"), so each saved sample is fully
represented in the database. This matches ClassiPyR's
`fill_unclassified_db()` convention, which downstream analysis relies
on, and exports to .mat as `NaN` (unreviewed). The backfill never
modifies existing rows, so incremental saves compose safely: images
saved to one class now are not touched when other images of the same
sample are saved to another class later.
