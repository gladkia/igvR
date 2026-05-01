# Constructor for GRangesQuantitativeTrack

`GRangesQuantitativeTrack` creates and `IGV` track for bed objects
imported using `rtracklayer`

## Usage

``` r
GRangesQuantitativeTrack(
  trackName,
  quantitativeData,
  color = "blue",
  trackHeight = 50,
  autoscale = TRUE,
  min = NA_real_,
  max = NA_real_,
  visibilityWindow = 1e+05
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- quantitativeData:

  A GRanges object with (at least) a "score" metadata column

- color:

  A CSS color name (e.g., "red" or "#FF0000")

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- autoscale:

  Autoscale track to maximum value in view

- min:

  Sets the minimum value for the data (y-axis) scale. Usually zero.

- max:

  Sets the maximum value for the data (y-axis) scale. This value is
  ignored if autoscale is TRUE

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

A GRangesQuantitativeTrack object

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

gr <- GRanges(tbl)
track <- GRangesQuantitativeTrack("GRangesQTest", gr)
```
