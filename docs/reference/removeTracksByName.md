# Remove named tracks

Remove named tracks

## Usage

``` r
# S4 method for class 'igvR'
removeTracksByName(obj, trackNames)
```

## Arguments

- obj:

  An object of class igvR

- trackNames:

  a character vector

## Value

A character vector

## See also

getTrackNames

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg19")
   showGenomicRegion(igv, "MEF2C")
     # create three arbitrary tracks
   base.loc <- 88883100
   tbl <- data.frame(chrom=rep("chr5", 3),
                     start=c(base.loc, base.loc+100, base.loc + 250),
                     end=c(base.loc + 50, base.loc+120, base.loc+290),
                     name=c("a", "b", "c"),
                     score=runif(3),
                     strand=rep("*", 3),
                     stringsAsFactors=FALSE)
   track.1 <- DataFrameAnnotationTrack("track.1", tbl, color="red", displayMode="SQUISHED")
   track.2 <- DataFrameAnnotationTrack("track.2", tbl, color="blue", displayMode="SQUISHED")
   track.3 <- DataFrameAnnotationTrack("track.3", tbl, color="green", displayMode="SQUISHED")
   displayTrack(igv, track.1)
   displayTrack(igv, track.2)
   displayTrack(igv, track.3)
   removeTracksByName(igv, "track.2")
     #----------------------------------------
     # bulk removal of the remaining tracks,
     # but leave the h19 reference track
     #----------------------------------------
   removeTracksByName(igv, getTrackNames(igv)[-1])
   }
```
