# test_igvR.R
#------------------------------------------------------------------------------------------------------------------------
library(RUnit)
library(igvR)
library(GenomicRanges)
library(VariantAnnotation)
#------------------------------------------------------------------------------------------------------------------------
testBrowserFile <- "/users/paul/github/igvR/inst/browserCode/testImmediate/testImmediate.html"

if(interactive()){
   if(!exists("igv")){
      igv <- igvR(browserFile=testBrowserFile, quiet=FALSE) # portRange=9000:9020)
      setBrowserWindowTitle(igv, "igvR")
      checkTrue(all(c("igvR", "BrowserVizClass") %in% is(igv)))
      } # exists
   } # interactive
#------------------------------------------------------------------------------------------------------------------------
