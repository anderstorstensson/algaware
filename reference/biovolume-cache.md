# Per-ROI biovolume cache

[`summarize_biovolumes()`](https://nodc-sweden.github.io/ifcb-algaware/reference/summarize_biovolumes.md)
re-reads every feature CSV and .hdr file from disk each time it runs,
even though relabelling ROIs only changes the class labels: the per-ROI
biovolume (from the feature files) and the per-sample analysed volume
(from the .hdr files) never change after download. These functions read
that immutable data once at load time and cache it in memory
(`rv$biovolume_cache`), so summary recomputation – after corrections,
sample exclusions, or at report time – becomes an in-memory aggregation
instead of a full re-read of the storage folder.

## Details

The per-class diatom decision (which selects the carbon conversion
formula) comes from the curated `is_diatom` column of the taxa lookup;
classes not covered there fall back to a WoRMS API lookup, cached per
class in `rv$diatom_status` so only classes not seen before in the
session trigger a network request.
