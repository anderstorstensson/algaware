# Dashboard metadata cache

Fetching metadata previously downloaded the full dashboard export on
every click, which grows with the archive. These helpers cache the last
download as an RDS file in local storage and, on the next fetch,
download only bins sampled on or after the newest cached day, replacing
the overlap. A full refetch happens automatically when the dashboard URL
or dataset changes, or on request (`force_full = TRUE`).
