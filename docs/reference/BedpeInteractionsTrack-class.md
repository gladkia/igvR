# Constructor for BedpeInteractionsTrack

`BedpeInteractionsTrack` creates an `IGV` track for two-location
annotations

## Usage

``` r
BedpeInteractionsTrack(
  trackName,
  table,
  color = "darkBlue",
  trackHeight = 50,
  displayMode = "EXPANDED",
  visibilityWindow = 1e+05
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- table:

  data.frame of 6 or more columns

- color:

  A css color name (e.g., "red" or "#FF0000"

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- displayMode:

  "COLLAPSED", "SQUISHED" or "EXPANDED". Spelling and case must be
  precise.

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

A BedpeInteractionsTrack object

## Examples

``` r

    #----------------------------
    #  first, from a local file
    #----------------------------

  file <- system.file(package="igvR", "extdata", "sixColumn-demo1.bedpe")
  tbl.bedpe <- read.table(file, sep="\t", as.is=TRUE, header=TRUE)
  dim(tbl.bedpe)  #  32 6
#> [1] 32  6
  track <- BedpeInteractionsTrack("bedpe-6", tbl.bedpe)

    #------------------------------------------
    #  show the relevant portion of the genome
    #------------------------------------------

  shoulder <- 10000
  if(interactive()){
     igv <- igvR()
     setGenome(igv, "hg38")
     setBrowserWindowTitle(igv, "Paired End Demo")
     roi <- with(tbl.bedpe, sprintf("%s:%d-%d", chrom1[1], min(start1)-shoulder, max(end2) + shoulder))
     showGenomicRegion(igv, roi)
     displayTrack(igv, track)
     }
```
