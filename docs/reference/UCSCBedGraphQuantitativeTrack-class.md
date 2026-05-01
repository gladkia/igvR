# Constructor for UCSCBedGraphQuantitativeTrack

`UCSCBedGraphQuantitativeTrack` creates an `IGV` track for bedGraph
objects imported with `rtracklayer`

## Usage

``` r
UCSCBedGraphQuantitativeTrack(
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

A UCSCBedGraphQuantitativeTrack object

## Details

Detailed description goes here

## Examples

``` r

bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
gr.bedGraph <- rtracklayer::import(bedGraph.filepath)
track <- UCSCBedGraphQuantitativeTrack("UCSCBedGraphTest", gr.bedGraph)

if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   setBrowserWindowTitle(igv, "UCSC BedGraph demo")
   displayTrack(igv, track)
   Sys.sleep(1)  # pause before zoomin
   showGenomicRegion(igv, "chr18:59,103,373-59,105,673")
   }
```
