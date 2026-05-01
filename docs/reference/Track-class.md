# Constructor for Track

Constructor for Track

## Usage

``` r
Track(
  trackType = c("annotation", "quantitative", "alignment", "variant", "gwas"),
  sourceType = c("file", "gcs", "ga4gh"),
  fileFormat = c("bed", "gff", "gff3", "gtf", "wig", "bigWig", "bedGraph", "bam", "vcf",
    "seg"),
  trackName,
  onScreenOrder,
  color,
  height,
  autoTrackHeight,
  minTrackHeight,
  maxTrackHeight,
  visibilityWindow
)
```

## Arguments

- trackType:

  One of "annotation", "quantitative", "variant".

- sourceType:

  Only "file" is currently supported.

- fileFormat:

  One of "bed", "bedGraph", "vcf"

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- onScreenOrder:

  Numeric, for explicit placement of track within the current set.

- color:

  A CSS color name (e.g., "red" or "#FF0000")

- height:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- autoTrackHeight:

  If true, then track height is adjusted dynamically, within the bounds
  set by minHeight and maxHeight, to accomdodate features in view

- minTrackHeight:

  In pixels, minimum allowed

- maxTrackHeight:

  In pixels, maximum allowed

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

An object of class Track

## References

<https://github.com/igvteam/igv.js/wiki/Tracks>

<https://www.w3schools.com/cssref/css_colors.asp>
