# Constructor for CramTrack

`CramTrack` creates an `IGV` track for remote CRAM files

## Usage

``` r
CramTrack(
  trackName,
  cramUrl,
  indexUrl,
  trackHeight = 50,
  visibilityWindow = 30000,
  color = "gray"
)
```

## Arguments

- trackName:

  A character string, used as track label by igv.

- cramUrl:

  The URL of a cram file.

- indexUrl:

  The URL of a cram index file (.crai).

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000.

- visibilityWindow:

  Maximum window size in base pairs for which alignments are displayed.
  Defaults to 30 kb.

- color:

  A character string, either a recognized color ("red") or a hex string.

## Value

A CramTrack object
