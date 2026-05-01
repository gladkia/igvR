# Constructor for DataFrameAnnotationTrack

`DataFrameAnnotationTrack` creates an `IGV` track for bed objects
imported using `rtracklayer`

## Usage

``` r
DataFrameAnnotationTrack(
  trackName,
  annotation,
  color = "",
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

  A base R `data.frame`

- color:

  A CSS color name (e.g., "red" or "#FF0000"), leave as default empty
  string if supplying bed9 format with itemRgb.

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

A DataFrameAnnotationTrack object

## Details

Detailed description goes here

## Examples

``` r
base.loc <- 88883100
tbl <- data.frame(chrom=rep("chr5", 3),
                  start=c(base.loc, base.loc+100, base.loc + 250),
                  end=c(base.loc + 50, base.loc+120, base.loc+290),
                  name=c("a", "b", "c"),
                  score=runif(3),
                  strand=rep("*", 3),
                  stringsAsFactors=FALSE)

track <- DataFrameAnnotationTrack("data.frame demo", tbl)

if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   setBrowserWindowTitle(igv, "DataFrameAnnotationTrack demo")
   displayTrack(igv, track)
   roi <- sprintf("%s:%d-%d", tbl$chrom[1], min(tbl$start)-100, max(tbl$start) + 100)
   showGenomicRegion(igv, roi)
   Sys.sleep(1)
   zoomOut(igv)
   }
```
