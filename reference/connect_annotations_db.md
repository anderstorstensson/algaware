# Open a connection to an annotations database

Sets a busy timeout so a concurrent writer (e.g. ClassiPyR holding a
short write lock on the shared database) makes us wait briefly instead
of failing immediately with "database is locked".

## Usage

``` r
connect_annotations_db(db_path)
```

## Arguments

- db_path:

  Path to the SQLite database file.

## Value

A DBI connection object.
