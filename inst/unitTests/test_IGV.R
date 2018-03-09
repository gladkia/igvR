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
test_showGenomicRegion <- function()
{
   printf("--- test_showGenomicRegion")

   checkTrue(ready(igv))

   setGenome(igv, "hg38")
   checkEquals(getGenomicRegion(igv), "chr1:1-248,956,422")
   new.region <- "chr5:88,866,900-88,895,833"
   showGenomicRegion(igv, new.region)
   checkEquals(getGenomicRegion(igv), new.region)

} # test_showGenomicRegion
#------------------------------------------------------------------------------------------------------------------------
test_loadSimpleBedTrackDirect <- function()
{
   printf("--- test_test_loadSimpleBedTrackDirect")

   checkTrue(ready(igv))

   setGenome(igv, "hg38")
   new.region <- "chr5:88,882,214-88,884,364"
   showGenomicRegion(igv, new.region)

   base.loc <- 88883100
   tbl <- data.frame(chrom=rep("chr5", 3),
                     start=c(base.loc, base.loc+100, base.loc + 250),
                     end=c(base.loc + 50, base.loc+120, base.loc+290),
                     name=c("a", "b", "c"),
                     score=runif(3),
                     strand=rep("*", 3),
                     stringsAsFactors=FALSE)

   track <- DataAnnotationTrack("dataframeTest", tbl)

   displayTrack(igv, track)


} # test_loadSimpleBedTrackDirect
#------------------------------------------------------------------------------------------------------------------------

