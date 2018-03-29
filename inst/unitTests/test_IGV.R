# test_IGV.R
#------------------------------------------------------------------------------------------------------------------------
library(RUnit)
library(IGV)
library(GenomicRanges)
library(VariantAnnotation)
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

   test_getShowGenomicRegion()

   test_displayVcfObject()
   test_displayVcfUrl()

   test_displayDataFrameAnnotationTrack()
   test_displayUCSCBedAnnotationTrack()

   test_displayDataFrameQuantitativeTrack()
   test_displayUCSCBedGraphQuantitativeTrack()

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
   checkEquals(getGenomicRegion(igv)$string, "chr1:1-248,956,422")

   setGenome(igv, "hg19")
   Sys.sleep(4)
   roi <- getGenomicRegion(igv)$string
   checkTrue(roi == "chr1:1-249,250,620" | roi == "chr1:1-249,250,621")

   setGenome(igv, "mm10")
   Sys.sleep(4)
   roi <- getGenomicRegion(igv)$string
   checkTrue(roi == "chr1:1-195,471,970" | roi == "chr1:1-195,471,971")

   setGenome(igv, "tair10")  #
   Sys.sleep(4)
   roi <- getGenomicRegion(igv)$string
   checkTrue(roi == "1:1-30,427,670" | roi == "1:1-30,427,671")

} # test_setGenome
#------------------------------------------------------------------------------------------------------------------------
test_getShowGenomicRegion <- function()
{
   printf("--- test_showGenomicRegion")

   checkTrue(ready(igv))

   setGenome(igv, "hg38")
   Sys.sleep(5)
   x <- getGenomicRegion(igv)
   checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
   checkEquals(x$chrom, "chr1")
   checkEquals(x$start, 1)
   checkTrue(x$end > 248956420 & x$end < 248956425)  # not sure why, but sometimes varies by 1 base
   checkTrue(grepl("chr1:1-248,956,42", x$string))   # leave off the last digit in the chromLoc string

   new.region.list <- list(chrom="chr5", start=88866900, end=88895833)
   new.region.string <- with(new.region.list, sprintf("%s:%d-%d", chrom, start, end))

      #--------------------------------------------------
      # send a list argument first
      #--------------------------------------------------

   showGenomicRegion(igv, new.region.list)
   Sys.sleep(5)
   x <- getGenomicRegion(igv)
   checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
   checkEquals(x$chrom, "chr5")
   checkEquals(x$start, 88866900)
   checkEquals(x$end, 88895833)
   checkEquals(x$string, "chr5:88,866,900-88,895,833")

      # reset the location
   showGenomicRegion(igv, "MYC")
   Sys.sleep(5)
   x <- getGenomicRegion(igv)
   checkEquals(x$chrom, "chr8")

      # send the string, repeat the above tests
   showGenomicRegion(igv, new.region.string)
   Sys.sleep(5)
   x <- getGenomicRegion(igv)
   checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
   checkEquals(x$chrom, "chr5")
   checkEquals(x$start, 88866900)
   checkEquals(x$end, 88895833)
   checkEquals(x$string, "chr5:88,866,900-88,895,833")

} # test_showGenomicRegion
#------------------------------------------------------------------------------------------------------------------------
test_displaySimpleBedTrackDirect <- function()
{
   printf("--- test_test_displaySimpleBedTrackDirect")

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

   track <- DataFrameAnnotationTrack("dataframeTest", tbl)

   displayTrack(igv, track)

} # test_displaySimpleBedTrackDirect
#------------------------------------------------------------------------------------------------------------------------
# in contrast to test_displayVcfUrl
test_displayVcfObject <- function()
{
   printf("--- test_displayVcfObject")
   setGenome(igv, "hg19")

   f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
   file.exists(f) # [1] TRUE
   vcf <- readVcf(f, "hg19")
      # get oriented around the contents of this vcf

   start <- 50586118
   end   <- 50633733
   rng <- GRanges(seqnames="22", ranges=IRanges(start=start, end=end))
                                               # names=c("gene_79087", "gene_644186")))
   vcf.sub <- readVcf(f, "hg19", param=rng)
   track <- VariantTrack("chr22-tiny", vcf.sub)
   showGenomicRegion(igv, sprintf("chr22:%d-%d", start-1000, end+1000))
   displayTrack(igv, track)
   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on


} # test_displayVcfObject
#------------------------------------------------------------------------------------------------------------------------
test_displayVcfUrl <- function()
{
   printf("--- test_displayVcfUrl")

   setGenome(igv, "hg19")
   Sys.sleep(5)   # wait for igv to render

   data.url <- "http://trena.systemsbiology.net/ampad/SCH_11923_B01_GRM_WGS_2017-04-27_10.recalibrated_variants.vcf.gz"
   index.url <- sprintf("%s.tbi", data.url)
   url <- list(data=data.url, index=index.url)
   showGenomicRegion(igv, "chr10:59,950,001-59,952,018")
   track <- VariantTrack("AMPAD chr10", url, displayMode="SQUISHED")
   displayTrack(igv, track)

      # change the colors, squish the display
   track.colored <- VariantTrack("AMPAD chr10 colors", url, displayMode="EXPANDED",
                                  locationColor="purple",
                                  homvarColor="brown",
                                  hetvarColor="green",
                                  homrefColor="yellow")

   displayTrack(igv, track.colored)
   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

} # test_displayVcfUrl
#------------------------------------------------------------------------------------------------------------------------
# first use a rich, 5-row, 12-column bed file conveniently provided by rtracklayer
# this has all the structure described here: https://genome.ucsc.edu/FAQ/FAQformat.html#format1
test_displayDataFrameAnnotationTrack <- function()
{
   printf("--- test_displayDataFrameAnnotationTrack")

   setGenome(igv, "hg19")
   Sys.sleep(3)  # allow time for the browser to create and load the reference tracks

      # first, the full 12-column form
   bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
   checkTrue(file.exists(bed.filepath))
   tbl.bed <- read.table(bed.filepath, sep="\t", as.is=TRUE, skip=2)
   colnames(tbl.bed) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
                          "thickStart", "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts")

   track.df <- DataFrameAnnotationTrack("bed.12col", tbl.bed)

   showGenomicRegion(igv, "chr7:127470000-127475900")
   displayTrack(igv, track.df)

   Sys.sleep(3)   # provide a chance to see the chr7 region before moving on to the chr9
   showGenomicRegion(igv, "chr9:127474000-127478000")
   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

      # now a simple 3-column barebones data.frame, in the same two regions as above

   chroms <- rep("chr7", 3)
   starts <- c(127471000, 127472000, 127473000)
   ends   <- starts + as.integer(100 * runif(3))
   tbl.chr7 <- data.frame(chrom=chroms, start=starts, end=ends, stringsAsFactors=FALSE)

   chroms <- rep("chr9", 30)
   starts <- seq(from=127475000, to=127476000, length.out=30)
   ends   <- starts + as.integer(100 * runif(30))
   tbl.chr9 <- data.frame(chrom=chroms, start=starts, end=ends, stringsAsFactors=FALSE)



   tbl.bed3 <- rbind(tbl.chr7, tbl.chr9)
   track.df2 <- DataFrameAnnotationTrack("bed.3col", tbl.bed3, color="green",
                                         displayMode="EXPANDED")

   showGenomicRegion(igv, "chr7:127470000-127475900")
   displayTrack(igv, track.df2)
   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

   showGenomicRegion(igv, "chr9:127474000-127478000")
   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on
   return(TRUE)

} # test_displayDataFrameAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayUCSCBedAnnotationTrack <- function()
{
   printf("--- test_displayUCSCBedAnnotationTrack")

   setGenome(igv, "hg19")
   Sys.sleep(3)  # allow time for the browser to create and load the reference tracks

   bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
   checkTrue(file.exists(bed.filepath))
   gr.bed <- import(bed.filepath)
   checkTrue(all(c("UCSCData", "GRanges") %in% is(gr.bed)))

   track.ucscBed <- UCSCBedAnnotationTrack("UCSCBed", gr.bed)

   showGenomicRegion(igv, "chr7:127470000-127475900")
   displayTrack(igv, track.ucscBed)

   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

   showGenomicRegion(igv, "chr9:127474000-127478000")
   Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

   return(TRUE)

} # test_displayUCSCBedAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayDataFrameQuantitativeTrack <- function()
{
   printf("--- test_displayDataFrameQuantitativeTrack")

   setGenome(igv, "hg19")
   Sys.sleep(3)  # allow time for the browser to create and load the reference tracks

   bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
   checkTrue(file.exists(bedGraph.filepath))

      # one metadata line at the top, without leading comment character. skip it.
   tbl.bg <- read.table(bedGraph.filepath, sep="\t", as.is=TRUE, skip=1)
   colnames(tbl.bg) <- c("chrom", "chromStart", "chromEnd", "score")

   track.bg0 <- DataFrameQuantitativeTrack("bedGraph data.frame", tbl.bg)
   displayTrack(igv, track.bg0)
   Sys.sleep(1)

      # now look at all three regions contained in the bedGraph data
   showGenomicRegion(igv, "chr19:59100000-59105000");  Sys.sleep(3)
   showGenomicRegion(igv, "chr18:59100000-59110000");  Sys.sleep(3)
   showGenomicRegion(igv, "chr17:59100000-59109000");  Sys.sleep(3)

} # test_displayDataFrameQuantitativeTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayUCSCBedGraphQuantitativeTrack <- function()
{
   printf("--- test_displayUCSCBedGraphQuantitativeTrack")

   setGenome(igv, "hg19")
   Sys.sleep(3)  # allow time for the browser to create and load the reference tracks

   bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
   checkTrue(file.exists(bedGraph.filepath))

   gr.bed <- import(bedGraph.filepath)
   checkTrue(class(gr.bed) == "UCSCData")   # UCSC BED format
   track.bg1 <- UCSCBedGraphQuantitativeTrack("rtracklayer bedGraph obj", gr.bed,  color="blue")

   displayTrack(igv, track.bg1)
   Sys.sleep(1)

      # now look at all three regions contained in the bedGraph data
   showGenomicRegion(igv, "chr19:59100000-59105000");  Sys.sleep(3)
   showGenomicRegion(igv, "chr18:59100000-59110000");  Sys.sleep(3)
   showGenomicRegion(igv, "chr17:59100000-59109000");  Sys.sleep(3)

} # test_displayUCSCBedGraphQuantitativeTrack
#------------------------------------------------------------------------------------------------------------------------
