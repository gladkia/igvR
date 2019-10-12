## ----setup, include = FALSE----------------------------------------------
options(width=120)
knitr::opts_chunk$set(
   collapse = TRUE,
   eval=interactive(),
   echo=TRUE,
   comment = "#>"
)


## ---- eval=TRUE, echo=FALSE----------------------------------------------
knitr::include_graphics("igvR-basicDemo.png")


## ----loadLibraries,  results='hide'--------------------------------------
library(igvR)


## ----createLoad, results='hide'------------------------------------------
igv <- igvR()
setBrowserWindowTitle(igv, "simple igvR demo")
getSupportedGenomes(igv)
setGenome(igv, "hg38")


## ----initialDisplay,  results='hide'-------------------------------------
showGenomicRegion(igv, "MYC")


## ----simple data.frame,  results='hide'----------------------------------
loc <- getGenomicRegion(igv)

tbl.bed <- data.frame(chrom=loc$chrom, start=loc$start + 2000, end=loc$end-2000, stringsAsFactors=FALSE)
track <- DataFrameAnnotationTrack("simple bed", tbl.bed, color="random")
displayTrack(igv, track)


## ----bedgraph-like data.frame,  results='hide'---------------------------
loc <- getGenomicRegion(igv)
starts <- seq(loc$start, loc$end, by=50)
ends   <- starts + 49
values <- jitter(rep(10, length(starts)), amount=8)
tbl.bedGraph <- data.frame(chrom=rep("chr8", length(starts)), start=starts, end=ends,
                           value=values, stringsAsFactors=FALSE)

track <- DataFrameQuantitativeTrack("bedGraph", tbl.bedGraph, color="red", autoscale=TRUE)
displayTrack(igv, track)

track <- DataFrameQuantitativeTrack("bedGraph-explicitly-scaled", tbl.bedGraph, color="blue",
                                    autoscale=FALSE, min=3, max=8)
displayTrack(igv, track)



## ----zoom out,  results='hide'-------------------------------------------
loc <- getGenomicRegion(igv)
half.span <- round((loc$end-loc$start)/2)

new.region <- with(loc, sprintf("%s:%d-%d", chrom, start-half.span, end+half.span))
showGenomicRegion(igv, new.region)


## ----sessionInfo---------------------------------------------------------
sessionInfo()

