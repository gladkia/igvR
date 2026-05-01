# Constructor for igvAnnotationTrack

Constructor for igvAnnotationTrack

## Usage

``` r
igvAnnotationTrack(
  trackName,
  annotation,
  fileFormat = c("bed"),
  color = "gray",
  displayMode = c("SQUISHED", "COLLAPSED", "EXPANDED"),
  sourceType = "file",
  trackHeight = 30,
  expandedRowHeight = 30,
  squishedRowHeight = 15,
  maxRows = 500,
  searchable = FALSE,
  visibilityWindow = 1e+05
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- annotation:

  An opague type, currently either a data.frame, GRanges, or UCSCBed
  object from rtracklayer.

- fileFormat:

  Only "bed" is currently supported.

- color:

  A CSS color name (e.g., "red" or "#FF0000")

- displayMode:

  "COLLAPSED", "EXPANDED", or "SQUISHED"

- sourceType:

  Only "file" sources are currently supported.

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- expandedRowHeight:

  Height of each row of features in "EXPANDED" mode.

- squishedRowHeight:

  Height of each row of features in "SQUISHED" mode, for compact
  viewing.

- maxRows:

  of features to display

- searchable:

  If TRUE, labels on annotation elements may be used in search

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

An igvAnnotationTrack object
