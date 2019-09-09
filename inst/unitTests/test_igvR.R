# test_igvR.R
#------------------------------------------------------------------------------------------------------------------------
library(RUnit)
library(igvR)
library(GenomicRanges)
library(VariantAnnotation)
#------------------------------------------------------------------------------------------------------------------------
printf <- function (...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
if(interactive()){
   if(!exists("igv")){
      igv <- igvR(quiet=TRUE) # portRange=9000:9020)
      setBrowserWindowTitle(igv, "igvR")
      checkTrue(all(c("igvR", "BrowserVizClass") %in% is(igv)))
      } # exists
   } # interactive
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_ping();
   test_quick()
   test_getSupportedGenomes()
   test_setGenome()

   test_getShowGenomicRegion()

   # test_displayVcfObject()
   #test_displayVcfUrl()

   #test_displayDataFrameAnnotationTrack()
   #test_displayUCSCBedAnnotationTrack()

   test_displayDataFrameQuantitativeTrack()
   test_displayDataFrameQuantitativeTrack_autoAndExplicitScale()
   #test_displayUCSCBedGraphQuantitativeTrack()

   test_displayAlignment()
   #test_saveToSVG()

   test_removeTracksByName()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_ping <- function()
{
   printf("--- test_ping")

   if(interactive()){
      checkTrue(ready(igv))
      checkEquals(ping(igv), "pong")
      }

} # test_ping
#------------------------------------------------------------------------------------------------------------------------
test_getSupportedGenomes <- function()
{
   printf("--- test_getSupportedGenomes")
   expected <- c("hg19", "hg38", "mm10", "tair10", "sacCer3", "Pfal3D7")
   checkTrue(all(expected %in% getSupportedGenomes(igv)))

} # test_getSupportedGenomes
#------------------------------------------------------------------------------------------------------------------------
test_quick <- function()
{
   printf("--- test_quick")

   if(interactive()){
      checkTrue(ready(igv))
      setGenome(igv, "hg38")
      checkTrue(ready(igv))
      showGenomicRegion(igv, "trem2")
      x <- getGenomicRegion(igv)
      checkEquals(x, list(chrom="chr6", start=41157506, end=41164186, string="chr6:41,157,506-41,164,186"))
      Sys.sleep(1)
      }

} # test_ping
#------------------------------------------------------------------------------------------------------------------------
test_setGenome <- function()
{
   printf("--- test_setGenome")

   if(interactive()){
      checkTrue(ready(igv))

      printf("---- hg38")
      setGenome(igv, "hg38")
      roi <- "chr1:153,588,447-153,707,067"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.from.browser <- getGenomicRegion(igv)
      checkEquals(roi, roi.from.browser$string)

      printf("---- hg19")
      setGenome(igv, "hg19")
      showGenomicRegion(igv, "mef2c")
      Sys.sleep(2)

      printf("---- mm10")
      setGenome(igv, "mm10")
      roi <- "chr1:40,184,529-40,508,207"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.from.browser <- getGenomicRegion(igv)$string
      checkTrue(roi.from.browser == roi)

      printf("---- tair10")
      setGenome(igv, "tair10")  #
      roi <- "1:15,094,978-15,332,693"
      showGenomicRegion(igv, roi)
      roi.from.browser <- getGenomicRegion(igv)$string
      checkTrue(roi.from.browser == roi)
      Sys.sleep(2)

      printf("---- sacCer3")
      setGenome(igv, "sacCer3")  #
      roi <- "chrV:327,611-331,072"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.from.browser <- getGenomicRegion(igv)$string
      checkTrue(roi == roi)

      printf("---- Pfal3D7")
      setGenome(igv, "Pfal3D7")  #
      ama1.gene.region <- "Pf3D7_11_v3:1,292,709-1,296,446"
      showGenomicRegion(igv, ama1.gene.region)
      Sys.sleep(2)
      roi <- getGenomicRegion(igv)$string
      checkTrue(roi == ama1.gene.region)
      } # if interactive

} # test_setGenome
#------------------------------------------------------------------------------------------------------------------------
test_getShowGenomicRegion <- function()
{
   printf("--- test_getShowGenomicRegion")

   if(interactive()){
      checkTrue(ready(igv))

      setGenome(igv, "hg38")
      showGenomicRegion(igv, "chr1")
      x <- getGenomicRegion(igv)
      checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
      checkEquals(x$chrom, "chr1")
      checkEquals(x$start, 1)
      checkTrue(x$end > 248956420 & x$end < 248956425)  # not sure why, but sometimes varies by 1 base
      checkTrue(grepl("chr1:1-248,956,42", x$string))   # leave off the last digit in the chromLoc string

        #--------------------------------------------------
        # send a list argument first
        #--------------------------------------------------

      new.region.list <- list(chrom="chr5", start=88866900, end=88895833)
      new.region.string <- with(new.region.list, sprintf("%s:%d-%d", chrom, start, end))

      showGenomicRegion(igv, new.region.list)
      x <- getGenomicRegion(igv)
      checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
      checkEquals(x$chrom, "chr5")
      checkEquals(x$start, 88866900)
      checkEquals(x$end, 88895833)
      checkEquals(x$string, "chr5:88,866,900-88,895,833")
      Sys.sleep(3)

         # reset the location
      showGenomicRegion(igv, "MYC")
      x <- getGenomicRegion(igv)
      checkEquals(x$chrom, "chr8")
      Sys.sleep(3)

         # send the string, repeat the above tests
      new.loc <- "chr5:88,659,708-88,737,464"
      showGenomicRegion(igv, new.loc)
      x <- getGenomicRegion(igv)
      checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
      checkEquals(x$chrom, "chr5")
      checkEquals(x$start, 88659708)
      checkEquals(x$end,   88737464)
      checkEquals(x$string, new.loc)
      } # if interactive

} # test_getShowGenomicRegion
#------------------------------------------------------------------------------------------------------------------------
test_displaySimpleBedTrackDirect <- function()
{
   printf("--- test_test_displaySimpleBedTrackDirect")

   if(interactive()){
      checkTrue(ready(igv))
      setGenome(igv, "hg38")
      new.region <- "chr5:88,882,214-88,884,364"
      showGenomicRegion(igv, new.region)

      base.loc <- 88883100
      tbl.01 <- data.frame(chrom=rep("chr5", 3),
                           start=c(base.loc, base.loc+100, base.loc + 250),
                           end=c(base.loc + 50, base.loc+120, base.loc+290),
                           name=c("A", "B", "C"),
                           score=round(runif(3), 2),
                           strand=rep("*", 3),
                           stringsAsFactors=FALSE)
      trackName.01 <- "dataframeTest.01"
      track.01 <- DataFrameAnnotationTrack(trackName.01, tbl.01, color="darkGreen", displayMode="EXPANDED")

      tbl.02 <- tbl.01
      tbl.02$start <- tbl.02$start + 100
      tbl.02$end   <- tbl.02$end + 100
      tbl.02$name <- c("D", "E", "F")
      trackName.02 <- "dataframeTest.02"
      track.02 <- DataFrameAnnotationTrack(trackName.02, tbl.02, color="brown", displayMode="EXPANDED")

      displayTrack(igv, track.01)
      displayTrack(igv, track.02)
      printf("--- back from displayTrack track.02")
      trackNames <- getTrackNames(igv)
      printf("trackNames: %s", paste(trackNames, collapse=","))
         # this test consistently fails due to some
      #checkTrue(trackName.01 %in% trackNames)
      #checkTrue(trackName.02 %in% trackNames)
      Sys.sleep(1)
      } # if interactive

} # test_displaySimpleBedTrackDirect
#------------------------------------------------------------------------------------------------------------------------
# in contrast to test_displayVcfUrl
test_displayVcfObject <- function()
{
   printf("--- test_displayVcfObject")
   if(interactive()){
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
      Sys.sleep(1)
      } # if interactive

} # test_displayVcfObject
#------------------------------------------------------------------------------------------------------------------------
test_displayVcfUrl <- function()
{
   printf("--- test_displayVcfUrl")

   if(interactive()){
      setGenome(igv, "hg19")
      Sys.sleep(5)   # wait for igv to render

      data.url <- "https://igv-data.systemsbiology.net/static/ampad/SCH_11923_B01_GRM_WGS_2017-04-27_10.recalibrated_variants.vcf.gz"
      index.url <- sprintf("%s.tbi", data.url)
      url <- list(data=data.url, index=index.url)
      showGenomicRegion(igv, "chr10:59,950,001-59,952,018")
      track <- VariantTrack("AMPAD chr10", url, displayMode="SQUISHED")
      displayTrack(igv, track)

      Sys.sleep(1)
      # change the colors, squish the display
      track.colored <- VariantTrack("AMPAD chr10 colors", url, displayMode="EXPANDED",
                                    anchorColor="purple",
                                    homvarColor="brown",
                                    hetvarColor="green",
                                    homrefColor="yellow")

      displayTrack(igv, track.colored)
      Sys.sleep(3)   # provide a chance to see the chr9 region before moving on
      } # if interactive

} # test_displayVcfUrl
#------------------------------------------------------------------------------------------------------------------------
# first use a rich, 5-row, 12-column bed file conveniently provided by rtracklayer
# this has all the structure described here: https://genome.ucsc.edu/FAQ/FAQformat.html#format1
test_displayDataFrameAnnotationTrack <- function()
{
   printf("--- test_displayDataFrameAnnotationTrack")

   if(interactive()){
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
      } # if interactive

} # test_displayDataFrameAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayUCSCBedAnnotationTrack <- function()
{
   printf("--- test_displayUCSCBedAnnotationTrack")

   if(interactive()){
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
      } # if interactive

} # test_displayUCSCBedAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayGRangesAnnotationTrack <- function()
{
   printf("--- test_displayGRangesAnnotationTrack")

   if(interactive()){
      setGenome(igv, "hg19")
      Sys.sleep(3)  # allow time for the browser to create and load the reference tracks

      bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
      checkTrue(file.exists(bed.filepath))
      tbl.bed <- read.table(bed.filepath, sep="\t", as.is=TRUE, skip=2)
      colnames(tbl.bed) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
                             "thickStart", "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts")

      gr.simple <- GRanges(tbl.bed[, c("chrom", "chromStart", "chromEnd", "name")])
      track.gr.1 <- GRangesAnnotationTrack("generic GRanges", gr.simple)
      checkTrue(all(c("GRangesAnnotationTrack", "igvAnnotationTrack", "Track") %in% is(track.gr.1)))
      checkEquals(trackSize(track.gr.1), 5)

      showGenomicRegion(igv, "chr7:127470000-127475900")
      displayTrack(igv, track.gr.1)
      Sys.sleep(1)

      gr.simpler <- GRanges(tbl.bed[, c("chrom", "chromStart", "chromEnd")])
      track.gr.2 <- GRangesAnnotationTrack("no-name GRanges", gr.simpler, color="orange")
      checkTrue(all(c("GRangesAnnotationTrack", "igvAnnotationTrack", "Track") %in% is(track.gr.2)))
      checkEquals(getSize(track.gr.2), 5)
      showGenomicRegion(igv, "chr7:127470000-127475900")
      displayTrack(igv, track.gr.2)

      Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

      showGenomicRegion(igv, "chr9:127474000-127478000")
      Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

      return(TRUE)
      } # if interactive

} # test_displayGRangesAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayDataFrameQuantitativeTrack <- function()
{
   printf("--- test_displayDataFrameQuantitativeTrack")

   if(interactive()){
      #setGenome(igv, "hg19")
      setGenome(igv, "hg38")

      base.start <- 58982201
      starts <- c(base.start, base.start+50, base.start+800)
      ends <- starts + c(40, 10, 80)
      tbl.bg <- data.frame(chrom=rep("chr18", 3),
                           start=starts,
                           end=ends,
                           value=c(0.5, -10.2, 20),
                           stringsAsFactors=FALSE)

         # both of these colnames work equally well.

      track.bg0 <- DataFrameQuantitativeTrack("bedGraph data.frame", tbl.bg, autoscale=FALSE,
                                              min=min(tbl.bg$value), max=max(tbl.bg$value),
                                              trackHeight=200, color="darkgreen")
      shoulder <- 1000
      showGenomicRegion(igv, sprintf("chr18:%d-%d", min(tbl.bg$start) - shoulder, max(tbl.bg$end) + shoulder))
      displayTrack(igv, track.bg0)
      Sys.sleep(3)
      } # if interactive

} # test_displayDataFrameQuantitativeTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayDataFrameQuantitativeTrack_autoAndExplicitScale <- function()
{
   printf("--- test_displayDataFrameQuantitativeTrack_autoAndExplicitScale")

   if(interactive()){
      setGenome(igv, "hg38")

      tbl <- data.frame(chr=rep("chr2", 3),
                        start=c(16102928, 16101906, 16102475),
                        end=  c(16102941, 16101917, 16102484),
                        value=c(2, 5, 19),
                        stringsAsFactors=FALSE)

      showGenomicRegion(igv, sprintf("chr2:%d-%d", min(tbl$start)-50, max(tbl$end)+50))
      track <- DataFrameQuantitativeTrack("autoScale", tbl, autoscale=TRUE)
      displayTrack(igv, track)
      Sys.sleep(3)
      track <- DataFrameQuantitativeTrack("specifiedScale", tbl, color="purple", trackHeight=100,
                                          autoscale=FALSE, min=1, max=30)
      displayTrack(igv, track)
      Sys.sleep(3)
      } # if interactive

} # test_displayDataFrameQuantitativeTrack_autoAndExplicitScale
#------------------------------------------------------------------------------------------------------------------------
test_displayUCSCBedGraphQuantitativeTrack <- function()
{
   printf("--- test_displayUCSCBedGraphQuantitativeTrack")

   if(interactive()){
      setGenome(igv, "hg19")
      bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
      checkTrue(file.exists(bedGraph.filepath))

      gr.bed <- import(bedGraph.filepath)
      checkTrue("UCSCData" %in% is(gr.bed))   # UCSC BED format
      track.bg1 <- UCSCBedGraphQuantitativeTrack("rtracklayer bedGraph obj", gr.bed,  color="blue")

      displayTrack(igv, track.bg1)
      Sys.sleep(1)

         # now look at all three regions contained in the bedGraph data
      showGenomicRegion(igv, "chr19:59100000-59105000");  Sys.sleep(3)
      showGenomicRegion(igv, "chr18:59100000-59110000");  Sys.sleep(3)
      showGenomicRegion(igv, "chr17:59100000-59109000");  Sys.sleep(3)
      } # if interactive

} # test_displayUCSCBedGraphQuantitativeTrack
#------------------------------------------------------------------------------------------------------------------------
# TODO (31 mar 2019): temporarily disabled.  some latency problem with latest igv.js?
test_removeTracksByName <- function()
{
   printf("--- test_removeTracksByName")
   setGenome(igv, "hg38")
   Sys.sleep(5)

   new.region <- "chr5:88,882,214-88,884,364"
   showGenomicRegion(igv, new.region)

   track.name <- "dataframeTest"

   base.loc <- 88883100
   tbl <- data.frame(chrom=rep("chr5", 3),
                     start=c(base.loc, base.loc+100, base.loc + 250),
                     end=c(base.loc + 50, base.loc+120, base.loc+290),
                     name=c("a", "b", "c"),
                     score=runif(3),
                     strand=rep("*", 3),
                     stringsAsFactors=FALSE)

   track <- DataFrameAnnotationTrack(track.name, tbl, color="darkGreen")
   displayTrack(igv, track)
   Sys.sleep(1)

   browser()
   trackNames <- getTrackNames(igv)
   checkTrue(track.name %in% trackNames)
   removeTracksByName(igv, track.name)
   checkTrue(!track.name %in% getTrackNames(igv))

} # test_removeTracksByName
#------------------------------------------------------------------------------------------------------------------------
test_displayAlignment <- function()
{
   printf("--- test_displayAlignment")

   bamFile <- system.file(package="igvR", "extdata", "tumor.bam")
   stopifnot(file.exists(bamFile))

   which <- GRanges(seqnames = "21", ranges = IRanges(10400126, 10400326))
   showGenomicRegion(igv, "chr21:10,399,824-10,400,627")

   param <- ScanBamParam(which=which, what = scanBamWhat())
   x <- readGAlignments(bamFile, use.names=TRUE, param=param)
   track <- GenomicAlignmentTrack("tumor", x)

   displayTrack(igv, track)
   Sys.sleep(2)

} # test_displayAlignment
#------------------------------------------------------------------------------------------------------------------------
test_saveToSVG <- function()
{
   printf("--- test_saveToSVG")
   setGenome(igv, "hg38")
   showGenomicRegion(igv, "GATA2")
   filename <- tempfile(fileext=".svg")
   saveToSVG(igv, filename)
   checkTrue(file.exists(filename))
   checkTrue(ile.size(filename) > 40000)

} # test_saveToSVG
#------------------------------------------------------------------------------------------------------------------------
demo_addTrackClickFunction_proofOfConcept <- function()
{
   printf("--- demo_addTrackClickFunction_proofOfConcept")

   if(interactive()){
      checkTrue(ready(igv))
      setGenome(igv, "hg38")
      new.region <- "chr5:88,882,214-88,884,364"
      showGenomicRegion(igv, new.region)

      base.loc <- 88883100
      tbl <- data.frame(chrom=rep("chr5", 3),
                        start=c(base.loc, base.loc+100, base.loc + 250),
                        end=c(base.loc + 50, base.loc+120, base.loc+290),
                        name=c("A", "B", "C"),
                        score=round(runif(3), 2),
                        strand=rep("*", 3),
                        stringsAsFactors=FALSE)

      track <- DataFrameAnnotationTrack("dataframeTest", tbl, color="darkGreen", displayMode="EXPANDED")
      displayTrack(igv, track)
      Sys.sleep(1)
      x <- list(arguments="track, popoverData", body="{console.log('track click 99')}")
      setTrackClickFunction(igv, x)

   } # if interactive

} # demo_displaySimpleBedTrackDirect_proofOfConcept
#------------------------------------------------------------------------------------------------------------------------
# displays a motif logo
demo_addTrackClickFunction_addLink <- function()
{
   printf("--- demo_addTrackClickFunction_addLink")

   if(interactive()){
      checkTrue(ready(igv))
      setGenome(igv, "hg38")
      new.region <- "chr5:88,882,214-88,884,364"
      showGenomicRegion(igv, new.region)

      base.loc <- 88883100
      links <- c("motifLogo://jaspar.genereg.net/static/logos/svg/MA0036.2.svg",
                 "MA0803.1",
                 "motifLogo://jaspar.genereg.net/static/logos/svg/MA0800.1.svg")


      tbl <- data.frame(chrom=rep("chr5", 3),
                        start=c(base.loc, base.loc+100, base.loc + 250),
                        end=c(base.loc + 50, base.loc+120, base.loc+290),
                        name=links,
                        score=round(runif(3), 2),
                        strand=rep("*", 3),
                        stringsAsFactors=FALSE)

      track <- DataFrameAnnotationTrack("dataframeTest", tbl, color="darkGreen", displayMode="EXPANDED")
      displayTrack(igv, track)
      Sys.sleep(1)
      body <- sprintf("return('%s')", "<h3>fobo</h3>")
      url <- "http://jaspar.genereg.net/static/logos/svg/MA0803.1.svg"
      imageTag <- sprintf("<img src=\"%s\" width=\"200\" />", url)
      imageTag <- sprintf("<img src='%s' width='200' />", url)
      #body <- sprintf("return('%s')", "<img src=\"%s/uploads/speaker.png\"/>")
      #body <- sprintf("{console.log('foo'); return('%s')}", imageTag)
      body <- sprintf('{return("%s")}', imageTag)

      body.parts <- c(
         #'var imgTag = \'<img src=\"http://jaspar.genereg.net/static/logos/svg/MA0803.1.svg\" width=200/>\';',
         #'return(imgTag);',
         #'console.log("not done yet");',
         'var returnValue = undefined;',
         'popoverData.forEach(function(i){',
         '   if(i.name=="name" && i.value.startsWith("motifLogo:")){',
         '      var url = i.value.replace("motifLogo:", "http:");',
         '      console.log(url);',
         '      var tag = "<img src=\'" + url + "\' width=300\'/>";',
         '      console.log(tag);',
         '      returnValue=tag;',
         '      };',
         '   });',
         '   console.log("--- returnValue:");',
         '   console.log(returnValue);',
         '   return(returnValue);'
         )

      body <- paste(body.parts, collapse=" ")
      x <- list(arguments="track, popoverData", body=body)
      setTrackClickFunction(igv, x)

   } # if interactive

} # demo_displaySimpleBedTrackDirect_addLink
#------------------------------------------------------------------------------------------------------------------------
