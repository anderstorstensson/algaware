# Read, transform, and rewrite an OOXML part as UTF-8

OOXML is UTF-8; read and write the part as raw bytes and only mark the
string as UTF-8 so the content never passes through the session's native
locale. Text connections (`file(path, encoding = "UTF-8")`) re-encode
to/from the native locale on read and write, which mangled every
non-ASCII character (Å/Ä/Ö, µ, –) into literal escape sequences on
non-UTF-8 hosts (e.g. servers running under a C/POSIX locale) —
silently, since the .docx still opened fine in Word.

## Usage

``` r
transform_ooxml_part(path, fun)
```

## Arguments

- path:

  Path to the XML part.

- fun:

  Function taking the file's text and returning the new text.

## Value

Invisibly TRUE if the file changed, FALSE otherwise.
