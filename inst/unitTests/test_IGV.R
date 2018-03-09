library(RUnit)
library(IGV)
#------------------------------------------------------------------------------------------------------------------------
if(!exists("igv")){
   igv <- IGV(portRange=9000:9020)
   setBrowserWindowTitle(igv, "IGV")
   checkTrue(all(c("IGV", "BrowserVizClass") %in% is(igv)))
   }
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_ping();
   test_setGenome()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_ping <- function()
{
   printf("--- test_ping")

   checkTrue(ready(igv))
   checkEquals(ping(igv), "pong")

} # test_ping
#------------------------------------------------------------------------------------------------------------------------
test_setGenome <- function()
{
   printf("--- test_setGenome")

   checkTrue(ready(igv))

   setGenome(igv, "hg38")
   Sys.sleep(4)
   checkEquals(getGenomicRegion(igv), "chr1:1-248,956,422")

   setGenome(igv, "hg19")
   Sys.sleep(4)
   checkEquals(getGenomicRegion(igv), "chr1:1-249,250,621")

   setGenome(igv, "mm10")
   Sys.sleep(4)
   checkEquals(getGenomicRegion(igv), "chr1:1-195,471,971")

   setGenome(igv, "tair10")  #
   Sys.sleep(4)
   checkEquals(getGenomicRegion(igv), "1:1-30,427,671")

} # test_setGenome
#------------------------------------------------------------------------------------------------------------------------
