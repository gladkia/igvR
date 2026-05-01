# turn mottif log popups on or off

Some tracks represent transcription factor binding sites, traditionally
represented as a motif logo. use this method to enable that capability -
which depends upon a properly constructed tbl.regions data.frame in a
DataFrameAnnotationTrack: in addition to the usual (and mandatory)
chrom, start, and end columns. To enable track-click popups over binding
site, tbl.regions data.frame must also have a "name" column, which this
format, by example: "MotifDb::Hsapiens-HOCOMOCOv10-MEF2C_HUMAN.H10MO.C"
The first part of the name, "MotifDb::", tells igv you want to view the
specified MotifDb pwm (motif logo, a matrix) when the binding site track
element is clicked.

Limitations: This method only works after a call to setGenome(igv, "your
genome of interest"). It only works with DataFrameAnnotationTrack
objects (for now)

## Usage

``` r
# S4 method for class 'igvR'
enableMotifLogoPopups(obj, status)
```

## Arguments

- obj:

  An object of class igvR

- status:

  TRUE or FALSE

## Value

No return value, called for side effects

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   new.region <- "chr5:88,882,214-88,884,364"
   showGenomicRegion(igv, new.region)
   base.loc <- 88883100
   element.names <- c("MotifDb::Hsapiens-HOCOMOCOv10-MEF2C_HUMAN.H10MO.C",
                      "fubar",
                      "MotifDb::Hsapiens-jaspar2018-MEF2C-MA0497.1")

   tbl.regions <- data.frame(chrom=rep("chr5", 3),
                             start=c(base.loc, base.loc+100, base.loc + 250),
                             end=c(base.loc + 50, base.loc+120, base.loc+290),
                             name=element.names,
                             score=round(runif(3), 2),
                             strand=rep("*", 3),
                             stringsAsFactors=FALSE)

   track <- DataFrameAnnotationTrack("dataframeTest", tbl.regions, color="darkGreen", displayMode="EXPANDED")
   displayTrack(igv, track)
   }
```
