# Constructor for GenomicAlignmentTrack

`GenomicAlignmentTrack` creates and `IGV` track for bed-like objects
expressed as GRanges

## Usage

``` r
GenomicAlignmentTrack(
  trackName,
  alignment,
  trackHeight = 50,
  visibilityWindow = 30000,
  color = "gray"
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- alignment:

  A GAlignments object

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

- color:

  A character string, either a reconized color ("red") or a hex string
  ("#FF8532")

## Value

A GenomicAlignmentTrack object

## Details

Detailed description goes here

## Examples

``` r

  bamFile <- system.file(package="igvR", "extdata", "tumor.bam")
  which <- GRanges(seqnames = "21", ranges = IRanges(10400126, 10400326))
  param <- ScanBamParam(which=which, what = scanBamWhat())
  x <- readGAlignments(bamFile, use.names=TRUE, param=param)
  track <- GenomicAlignmentTrack("tumor", x)
```
