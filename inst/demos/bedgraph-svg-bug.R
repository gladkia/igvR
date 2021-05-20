# bedgraph-svg-bug.R
#
# https://github.com/alessandriLuca issue
# https://github.com/paul-shannon/igvR/issues/13
# I have some trouble on svg saving from both the browser and from the code it give me the same
# results. So apparently when i load a bedgraph with negative value they are displayed on the
# browser, then i save everything on svg and here is the problem, positive value of bedgraph are
# fine, negative just disappear. Another funny thing is that in the thumbinal of the svg i can
# definetly see the negative value.
#
#------------------------------------------------------------------------------------------------------------------------
library(RUnit)
library(igvR)
library(GenomicRanges)
library(rtracklayer)
#------------------------------------------------------------------------------------------------------------------------
printf <- function (...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
if(!exists("igv")){
   igv <- igvR(quiet=TRUE) # portRange=9000:9020)
   setBrowserWindowTitle(igv, "bedgraph svg bug demo")
   checkTrue(all(c("igvR", "BrowserViz") %in% is(igv)))
   } # exists
#------------------------------------------------------------------------------------------------------------------------
demo <- function()
{
   setGenome(igv, "hg19")

   base.start <- 58982201
   starts <- c(base.start, base.start+50, base.start+800)
   ends <- starts + c(40, 10, 80)
   tbl.bg <- data.frame(chrom=rep("chr18", 3),
                        start=starts,
                        end=ends,
                        value=c(0.5, -10.2, 20),
                        stringsAsFactors=FALSE)

   track.bg0 <- DataFrameQuantitativeTrack("bedGraph data.frame", tbl.bg, autoscale=FALSE,
                                           min=min(tbl.bg$value), max=max(tbl.bg$value),
                                           trackHeight=200, color="darkgreen")
   shoulder <- 1000
   showGenomicRegion(igv, sprintf("chr18:%d-%d", min(tbl.bg$start) - shoulder, max(tbl.bg$end) + shoulder))
   displayTrack(igv, track.bg0)

  filename <- "demo.svg"
  saveToSVG(igv, filename)   # open local file in chrome

} # demo
#------------------------------------------------------------------------------------------------------------------------
