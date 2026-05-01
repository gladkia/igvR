# Constructor for UCSCBedAnnotationTrack

`UCSCBedAnnotationTrack` creates and `IGV` track for bed objects
imported using `rtracklayer`

## Usage

``` r
UCSCBedAnnotationTrack(
  trackName,
  annotation,
  color = "darkGrey",
  displayMode = "SQUISHED",
  trackHeight = 50,
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

  A UCSCData object imported by `rtracklayer`

- color:

  A CSS color name (e.g., "red" or "#FF0000")

- displayMode:

  "COLLAPSED", "SQUISHED" or "EXPANDED". Spelling and case must be
  precise.

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

A UCSCBedAnnotationTrack object

## Details

Detailed description goes here

## Examples

``` r

bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
gr.bed <- rtracklayer::import(bed.filepath)
track <- UCSCBedAnnotationTrack("UCSC bed", gr.bed,  color="blue", displayMode="SQUISHED")

if(interactive()){
    igv <- igvR()
    setGenome(igv, "hg38")
    setBrowserWindowTitle(igv, "UCSC bed10 demo")
    showGenomicRegion(igv, "chr7:127,469,879-127,476,276")
    displayTrack(igv, track)
    }
```
