# display the specified track in igv

display the specified track in igv

## Usage

``` r
# S4 method for class 'igvR'
displayTrack(obj, track, deleteTracksOfSameName = TRUE)
```

## Arguments

- obj:

  An object of class igvR

- track:

  An object of some terminal (leaf) subclass of Track

- deleteTracksOfSameName:

  logical, default TRUE

## Value

""

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   showGenomicRegion(igv, "MEF2C")
   base.loc <- 88883100
   tbl <- data.frame(chrom=rep("chr5", 3),
                     start=c(base.loc, base.loc+100, base.loc + 250),
                     end=c(base.loc + 50, base.loc+120, base.loc+290),
                     name=c("a", "b", "c"),
                     score=runif(3),
                     strand=rep("*", 3),
                     stringsAsFactors=FALSE)
   track <- DataFrameAnnotationTrack("dataframeTest", tbl, color="red",
                                      displayMode="EXPANDED")
   showGenomicRegion(igv, "chr5:88,881,962-88,885,045")
   displayTrack(igv, track)
   }
```
