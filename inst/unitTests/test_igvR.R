# test_igvR.R
#------------------------------------------------------------------------------------------------------------------------
library(RUnit)
library(igvR)
library(GenomicRanges)
library(VariantAnnotation)
#------------------------------------------------------------------------------------------------------------------------
printf <- function (...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
#interactive <- function() TRUE;
#------------------------------------------------------------------------------------------------------------------------
if(BrowserViz::webBrowserAvailableForTesting()){
   if(!exists("igv")){
      igv <- igvR(host=Sys.info()["nodename"], quiet=TRUE, portRange=12000:12020)
      setBrowserWindowTitle(igv, "igvR unit tests")
      checkTrue(all(c("igvR", "BrowserViz") %in% is(igv)))
      } # exists
   } # interactive
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_setGenome()

   setGenome(igv, "hg38")
   test_quick()
   test_getShowGenomicRegion()
   test_displaySimpleBedTrackDirect()
   test_displayDataFrameQuantitativeTrack()
   test_displayDataFrameQuantitativeTrack_autoAndExplicitScale()
   test_removeTracksByName()
   test_displayAlignment()
   test_saveToSVG()
   test_.writeMotifLogoImagesUpdateTrackNames()

   setGenome(igv, "hg19")
   test_displayVcfObject()

   #removeTracksByName(igv, getTrackNames(igv)[-1])
   #test_displayVcfUrl()

   removeTracksByName(igv, getTrackNames(igv)[-1])
   test_displayDataFrameAnnotationTrack()

   removeTracksByName(igv, getTrackNames(igv)[-1])
   test_displayUCSCBedAnnotationTrack()

   removeTracksByName(igv, getTrackNames(igv)[-1])
   test_displayGRangesAnnotationTrack()

   removeTracksByName(igv, getTrackNames(igv)[-1])
   test_displayUCSCBedGraphQuantitativeTrack()

   removeTracksByName(igv, getTrackNames(igv)[-1])
   setGenome(igv, "hg38")
   test_displayBedpeInteractions()

   removeTracksByName(igv, getTrackNames(igv)[-1])
   setGenome(igv, "hg19")
   test_displayGWAS.gwasFormat()
   test_displayGWAS.gwascatFormat()
   test_displayGWASUrlTrack()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_ping <- function()
{
   message(sprintf("--- test_ping"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))
      checkEquals(ping(igv), "pong")
      }

} # test_ping
#------------------------------------------------------------------------------------------------------------------------
# obsolete.  see test_genomeSpec.R
notest_getSupportedGenomes <- function()
{
   message(sprintf("--- test_getSupportedGenomes"))
   expected <- c("hg38", "hg19", "hg18", "mm10", "gorgor4", "pantro4", "panpan2", "susscr11", "bostau8", "canfam3",
                 "rn6", "danrer11", "danrer10", "dm6", "ce11", "saccer3",
                 "tair10", "pfal3d7")  # these last two are hosted on trena, aka gladki.pl/igvR
   setdiff(expected, getSupportedGenomes(igv))
   checkTrue(all(expected %in% getSupportedGenomes(igv)))

} # test_getSupportedGenomes
#------------------------------------------------------------------------------------------------------------------------
# assumes and depends upon the hg38 genome
test_quick <- function()
{
   message(sprintf("--- test_quick"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))
      setGenome(igv, "hg38")
      showGenomicRegion(igv, "trem2")
      x <- getGenomicRegion(igv)
      checkEquals(x$chrom, "chr6")
      checkEqualsNumeric(x$start, 41157507, tolerance=10)
      checkEqualsNumeric(x$end,   41164114, tolerance=10)
      checkEqualsNumeric(x$width, 6610, tolerance=10)
      checkEquals(gsub(",", "", x$string), sprintf("%s:%d-%d", x$chrom, x$start, x$end))
      showTrackLabels(igv, FALSE)
      showTrackLabels(igv, TRUE)
      Sys.sleep(1)
      }

} # test_ping
#------------------------------------------------------------------------------------------------------------------------
test_setGenome <- function()
{
   message(sprintf("--- test_setGenome"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))

      supported <- getSupportedGenomes(igv)
      checkTrue(length(supported) > 30)

      message(sprintf("---- hg38"))
      setGenome(igv, "hg38")
      roi <- "chr1:153,588,447-153,707,067"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.from.browser <- getGenomicRegion(igv)
      checkEquals(roi, roi.from.browser$string)

      message(sprintf("---- hg19"))
      setGenome(igv, "hg19")
      showGenomicRegion(igv, "mef2c")
      Sys.sleep(2)

      message(sprintf("---- mm10"))
      setGenome(igv, "mm10")
      roi <- "chr1:40,184,529-40,508,207"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.from.browser <- getGenomicRegion(igv)$string
      checkTrue(roi.from.browser == roi)

      message(sprintf("---- sacCer3"))
      setGenome(igv, "sacCer3")  #
      roi <- "chrV:327,611-331,072"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.from.browser <- getGenomicRegion(igv)$string
      checkTrue(roi == roi)


      for(genome in getSupportedGenomes(igv)){
         message(sprintf("---- %s", genome))
         setGenome(igv, genome);
         Sys.sleep(2)
         }

      } # if webBrowserAvailableForTesting

} # test_setGenome
#------------------------------------------------------------------------------------------------------------------------
# arabidopsis config:
#         reference: {id: "TAIR10",
#                fastaURL: "https://gladki.pl/igvR/tair10/Arabidopsis_thaliana.TAIR10.dna.toplevel.fa",
#                indexURL: "https://gladki.pl/igvR/tair10/Arabidopsis_thaliana.TAIR10.dna.toplevel.fa.fai",
#                aliasURL: "https://gladki.pl/igvR/tair10/chromosomeAliases.txt"
#                },
#         tracks: [
#           {name: 'Genes TAIR10',
#            type: 'annotation',
#            visibilityWindow: 500000,
#            url: "https://gladki.pl/igvR/tair10/TAIR10_genes.sorted.chrLowered.gff3.gz",
#            color: "darkred",
#            indexed: true,
#            height: 200,
#            displayMode: "EXPANDED"
#            },
#            ]
#  rhodobacter sphaeroides, rhos1290 config.
#   ~/github/igvR/misc/serveYourOwnFiles/rhos/
#                      rhodobacter-sphaeroides-demo.html
#
#   http://gladki.pl/igvR/rhos/GCF_000012905.2_ASM1290v2_genomic.fna.fai
#  4661026 May  9  2019 GCF_000012905.2_ASM1290v2_genomic.fna
#      226 May  9  2019 GCF_000012905.2_ASM1290v2_genomic.fna.fai
#  1378253 May  9  2019 GCF_000012905.2_ASM1290v2_genomic.fna.gz
#   421030 May  9  2019 GCF_000012905.2_ASM1290v2_genomic.gff.gz
#     3135 May  9  2019 GCF_000012905.2_ASM1290v2_genomic.gff.gz.tbi
#
# and from peng zhou, for maze:
# "id": "maize",
# "name": "Zea mays B73v4",
# "fastaURL": "https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.fasta",
# "indexURL": "https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.fasta.fai"
# {
# "name": "Genes",
# "format": "gff3",
# "url": "https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.gff.gz",
# "indexURL": "https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.gff.gz.tbi",
# }
test_setCustomGenome <- function()
{
   message(sprintf("--- test_setCustomGenome"))

   if(BrowserViz::webBrowserAvailableForTesting()){

          #----------------------------------------
          # first, arabidopsis at the isb
          #----------------------------------------
      checkTrue(ready(igv))
      setCustomGenome(igv,
                      id="hg38",
                      genomeName="Human (GRCh38/hg38)",
                      fastaURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa",
                      fastaIndexURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai",
                      cytobandURL="https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg38/cytoBandIdeo.txt",
                      chromosomeAliasURL=NA,
                      geneAnnotationName="Refseq Genes",
                      geneAnnotationURL="https://s3.amazonaws.com/igv.org.genomes/hg38/refGene.txt.gz")

      roi <- "MEF2C"
      showGenomicRegion(igv, roi)
      Sys.sleep(2)
      roi.2 <- getGenomicRegion(igv)$string
      checkTrue(roi.2 == roi)

          #---------------------------------------------
          # now, maize at the university of minnesota
          #---------------------------------------------
      checkTrue(ready(igv))
      setCustomGenome(igv,
                      id="Corn",
                      genomeName="Zea mays B73v4",
                      fastaURL="https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.fasta",
                      fastaIndexURL="https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.fasta.fai",
                      chromosomeAliasURL=NA,
                      cytobandURL=NA,
                      geneAnnotationURL="https://s3.msi.umn.edu/zhoup-igv-data/Zmays-B73/10.gff.gz")

      } # if webBrowserAvailableForTesting


} # test_setCustomGenome
#------------------------------------------------------------------------------------------------------------------------
test_getShowGenomicRegion <- function()
{
   message(sprintf("--- test_getShowGenomicRegion"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))

      setGenome(igv, "hg38")
      showGenomicRegion(igv, "chr1")
      x <- getGenomicRegion(igv)
      checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
      checkEquals(x$chrom, "chr1")
      checkEquals(x$start, 1)
      checkEqualsNumeric(x$end, 248956422, tolerance=10)  # not sure why, but sometimes varies by 1 base
      checkTrue(grepl("chr1:1-248,", x$string))   # leave off the last digit in the chromLoc string

        #--------------------------------------------------
        # send a list argument first
        #--------------------------------------------------

      new.region.list <- list(chrom="chr5", start=88866900, end=88895833)
      new.region.string <- with(new.region.list, sprintf("%s:%d-%d", chrom, start, end))

      showGenomicRegion(igv, new.region.string)
      x <- getGenomicRegion(igv)
      checkTrue(all(c("chrom", "start", "end", "string") %in% names(x)))
      checkEquals(x$chrom, "chr5")
      checkEqualsNumeric(x$start, 88866900, tolerance=3)
      checkEqualsNumeric(x$end, 88895833, tolerance=3)
      checkEquals(gsub(",", "", x$string), with(x, sprintf("%s:%d-%d", chrom, start, end)))
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
      checkEqualsNumeric(x$start, 88659708, tolerance=3)
      checkEqualsNumeric(x$end,   88737464, tolerance=3)
      checkEquals(x$string, new.loc)
      } # if interactive

} # test_getShowGenomicRegion
#------------------------------------------------------------------------------------------------------------------------
test_displaySimpleBedTrackDirect <- function()
{
   message(sprintf("--- test_displaySimpleBedTrackDirect"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))
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
      } # if interactive

} # test_displaySimpleBedTrackDirect
#------------------------------------------------------------------------------------------------------------------------
test_displaySimpleBedTrackDirect.withRowColors <- function()
{
   message(sprintf("--- test_displaySimpleBedTrackDirect.withRowColors"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))
      new.region <- "chr5:88,882,214-88,884,364"
      showGenomicRegion(igv, new.region)

      base.loc <- 88883100
      tbl.01 <- data.frame(chrom=rep("chr5", 3),
                           start=c(base.loc, base.loc+100, base.loc + 250),
                           end=c(base.loc + 50, base.loc+120, base.loc+290),
                           name=c("A", "B", "C"),
                           score=round(runif(3), 2),
                           strand=c("+", "-", "+"),
                           thickStart=c(base.loc+10, base.loc+110, base.loc+260),
                           thickEnd=c(base.loc+20, base.loc+130, base.loc+250),
                           itemRgb=c("0,255,128", "64,32,255", "128,64,0"),
                           stringsAsFactors=FALSE)
      trackName.01 <- "row colors 1"
      track.01 <- DataFrameAnnotationTrack(trackName.01, tbl.01, color="",
                                           displayMode="EXPANDED")

      tbl.02 <- tbl.01
      tbl.02$start <- tbl.02$start + 100
      tbl.02$end   <- tbl.02$end + 100
      tbl.02$name <- c("D", "E", "F")
      trackName.02 <- "row colors 2"
      track.02 <- DataFrameAnnotationTrack(trackName.02, tbl.02, color="", displayMode="EXPANDED")

      displayTrack(igv, track.01)
      displayTrack(igv, track.02)
      } # if interactive

} # test_displaySimpleBedTrackDirect.withRowColors
#------------------------------------------------------------------------------------------------------------------------
# in contrast to test_displayVcfUrl
test_displayVcfObject <- function()
{
   message(sprintf("--- test_displayVcfObject"))
   if(BrowserViz::webBrowserAvailableForTesting()){
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
      #Sys.sleep(3)

      track2 <- VariantTrack("chr22-smallWindow", vcf.sub, visibilityWindow=20000)
      displayTrack(igv, track2)
        # zoom in enough to see this track
      showGenomicRegion(igv, "chr22:50,603,724-50,616,127")
      trackNames <- getTrackNames(igv)

      expected <- c("Refseq Genes", "chr22-tiny", "chr22-smallWindow")
      checkTrue(all(expected %in% trackNames))
      #printf("trackNames: %s", paste(trackNames, collapse=","))
      #checkTrue("chr22-tiny" %in% trackNames)
      } # if interactive

} # test_displayVcfObject
#------------------------------------------------------------------------------------------------------------------------
explore.vcf.igvData.failure <- function()
{
   roi <- list(chrom="22", start=19949227, end=19951245)
   roi.string <- with(roi, sprintf("%s:%d-%d", chrom, start, end))
   region <- with(roi, GRanges(seqnames=chrom, IRanges(start, end)))

       #------------------------------------------------------------------------
       # first, with the appended random seed query, used by igv.js to ensure
       # the files are not 304's - unchanged, so not returned
       #------------------------------------------------------------------------

   url.data <-
       "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz?someRandomSeed=0.1uiyyunnlyh"
   url.index <-
       "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz.tbi?someRandomSeed=0.1uiyyunnlyh"

  f <- VcfFile(file=url.data, index=url.index)
  x <- readVcf(f, "hg19", region)
  length(x)

       #-------------------------------------------------------------------------
       # now, with just the real names in the url.  readVcf seems to work despite
       # being exposed to the 304 response
       #-------------------------------------------------------------------------

  url.data <- "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz"
  url.index <- "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz.tbi"

  f <- VcfFile(file=url.data, index=url.index)
  x <- readVcf(f, "hg19", region)
  length(x)



} # explore.vcf.igvData.failure
#------------------------------------------------------------------------------------------------------------------------
test_displayVcfUrl <- function()
{
   message(sprintf("--- test_displayVcfUrl"))

   url.data.size <- function(url){
      url.info <- system(sprintf("curl -I -L %s", url), intern=TRUE, ignore.stderr=TRUE)
      file.length.raw.text <- url.info[grep("Content-Length", url.info)]
         # for instance:  "Content-Length" "16048144961\r"
      as.numeric(sub("\\r", "", strsplit(file.length.raw.text, ": ")[[1]][2]))
      }

   url.exists <- function(url){
      url.data.size(url) > 0
      }

   url.augmented <- "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz?someRandomSeed=0.1uiyyunnlyh"
   url.exists(url.augmented)

   roi <- list(chrom="22", start=19949227, end=19951245)
   roi.string <- with(roi, sprintf("%s:%d-%d", chrom, start, end))
   region <- with(roi, GRanges(seqnames=chrom, IRanges(start, end)))

   url.1kg.data <- "https://s3.amazonaws.com/1000genomes/release/20130502/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz"
   url.1kg.index <- sprintf("%s.tbi", url.1kg.data)
   checkTrue(url.exists(url.1kg.data))
   checkTrue(url.exists(url.1kg.index))

   vcfFile <- VcfFile(url.1kg.data, index=url.1kg.index)
   vcf.1kg <- readVcf(vcfFile, "hg19", region)
   checkEquals(length(vcf.1kg), 75)
   mtx.geno <- geno(vcf.1kg)$GT
   checkEquals(dim(mtx.geno), c(75, 2504))

   url.ampad.data <- "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz"
   url.ampad.index <- sprintf("%s.tbi", url.ampad.data)
   checkTrue(url.exists(url.ampad.data))
   checkTrue(url.exists(url.ampad.index))

   vcfFile <- VcfFile(url.ampad.data, index=url.ampad.index)
   vcf.ampad <- readVcf(vcfFile, "hg19", region)
   checkEquals(length(vcf.ampad), 46)
   mtx.geno.local <- geno(vcf.ampad)$GT
   checkEquals(dim(mtx.geno.local), c(46, 1894))

   url.ampad.augmented.data <-
       "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz?someRandomSeed=0.1uiyyunnlyh"
   url.ampad.augmented.index <-
       "https://gladki.pl/igvR/ampad/NIA-1898/chr22.vcf.gz.tbi?someRandomSeed=0.1uiyyunnlyh"

   checkTrue(url.exists(url.ampad.augmented.data))
   checkTrue(url.exists(url.ampad.augmented.index))

   vcfFile <- VcfFile(url.ampad.augmented.data, index=url.ampad.augmented.index)
   vcf.ampad <- readVcf(vcfFile, "hg19", region)
   checkEquals(length(vcf.ampad), 46)
   mtx.geno.local <- geno(vcf.ampad)$GT
   checkEquals(dim(mtx.geno.local), c(46, 1894))


   if(BrowserViz::webBrowserAvailableForTesting()){
      setGenome(igv, "hg38")
      showGenomicRegion(igv, roi.string)
      url.1 <- list(data=url.1kg.data, index=url.1kg.index)
      track.1 <- VariantTrack("1kg COMPT", url.1, displayMode="COLLAPSED", visibilityWindow=5000)
      displayTrack(igv, track.1)

      url.2 <- list(data=url.ampad.data, index=url.ampad.index)
      track.2 <-VariantTrack("ampad COMPT", url.2, displayMode="COLLAPSED", visibilityWindow=5000)
      displayTrack(igv, track.2)

        # change the colors, squish the display
      track.colored <- VariantTrack("1kg colors",
                                    vcf=list(data=url.ampad.data,
                                            index=url.ampad.index),
                                    anchorColor="purple",
                                    homvarColor="red",
                                    hetvarColor="darkRed",
                                    homrefColor="darkGray",
                                    visibilityWindow=3000,
                                    displayMode="COLLAPSED")

      displayTrack(igv, track.colored)
      checkEquals(length(getTrackNames(igv)), 4)
      } # if interactive

} # test_displayVcfUrl
#------------------------------------------------------------------------------------------------------------------------
# first use a rich, 5-row, 12-column bed file conveniently provided by rtracklayer
# this has all the structure described here: https://genome.ucsc.edu/FAQ/FAQformat.html#format1
test_displayDataFrameAnnotationTrack <- function()
{
   message(sprintf("--- test_displayDataFrameAnnotationTrack"))

   if(BrowserViz::webBrowserAvailableForTesting()){
         # first, the full 12-column form
      bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
      checkTrue(file.exists(bed.filepath))
      tbl.bed <- read.table(bed.filepath, sep="\t", as.is=TRUE, skip=2)
      colnames(tbl.bed) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
                             "thickStart", "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts")

      track <- DataFrameAnnotationTrack("bed.12col", tbl.bed)

      loc.string <- sprintf("%s:%d-%d",
                            tbl.bed$chrom[1],
                            min(tbl.bed$chromStart)-1000,
                            max(tbl.bed$chromEnd) + 1000)
      showGenomicRegion(igv, loc.string)
      showGenomicRegion(igv, "chr7:127470000-127475900")
      displayTrack(igv, track)

      for(i in seq_len(nrow(tbl.bed))){
          title <- tbl.bed$name[i];
          track <- DataFrameAnnotationTrack(title, tbl.bed[i,,drop=FALSE])
          displayTrack(igv, track)
          }

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
      tbl.track2 <- DataFrameAnnotationTrack("bed.3col", tbl.bed3, color="green", displayMode="EXPANDED")

      showGenomicRegion(igv, "chr7:127470000-127475900")
      displayTrack(igv, tbl.track2)
      Sys.sleep(3)   # provide a chance to see the chr9 region before moving on

      showGenomicRegion(igv, "chr9:127474000-127478000")
      Sys.sleep(3)   # provide a chance to see the chr9 region before moving on
      return(TRUE)
      } # if interactive

} # test_displayDataFrameAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayUCSCBedAnnotationTrack <- function()
{
   message(sprintf("--- test_displayUCSCBedAnnotationTrack"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
      checkTrue(file.exists(bed.filepath))
      gr.bed <- import(bed.filepath)
      checkTrue(all(c("UCSCData", "GRanges") %in% is(gr.bed)))
      track.ucscBed <- UCSCBedAnnotationTrack("UCSCBed", gr.bed)
      displayTrack(igv, track.ucscBed)
      service(3000)
      showGenomicRegion(igv, "chr7:127470000-127475900")
      service(5000)
      showGenomicRegion(igv, "chr9:127474000-127478000")
      service(5000)
      return(TRUE)
      } # if interactive

} # test_displayUCSCBedAnnotationTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayGRangesAnnotationTrack <- function()
{
   message(sprintf("--- test_displayGRangesAnnotationTrack"))

   if(BrowserViz::webBrowserAvailableForTesting()){
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
      checkEquals(trackSize(track.gr.2), 5)
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
   message(sprintf("--- test_displayDataFrameQuantitativeTrack"))

   if(BrowserViz::webBrowserAvailableForTesting()){
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
      #Sys.sleep(5)
      } # if interactive

} # test_displayDataFrameQuantitativeTrack
#------------------------------------------------------------------------------------------------------------------------
test_displayDataFrameQuantitativeTrack_autoAndExplicitScale <- function()
{
   message(sprintf("--- test_displayDataFrameQuantitativeTrack_autoAndExplicitScale"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      tbl <- data.frame(chr=rep("chr2", 3),
                        start=c(16102928, 16101906, 16102475),
                        end=  c(16102941, 16101917, 16102484),
                        value=c(2, 5, 19),
                        stringsAsFactors=FALSE)

      showGenomicRegion(igv, sprintf("chr2:%d-%d", min(tbl$start)-50, max(tbl$end)+50))
      track <- DataFrameQuantitativeTrack("autoScale", tbl, autoscale=TRUE, trackHeight=100)
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
   message(sprintf("--- test_displayUCSCBedGraphQuantitativeTrack"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
      checkTrue(file.exists(bedGraph.filepath))

      gr.bed <- import(bedGraph.filepath)
      checkTrue("UCSCData" %in% is(gr.bed))   # UCSC BED format
      track.bg1 <- UCSCBedGraphQuantitativeTrack("rtracklayer bedGraph obj", gr.bed,  color="blue")

      displayTrack(igv, track.bg1)

         # now look at all three regions contained in the bedGraph data
      showGenomicRegion(igv, "chr19:59100000-59105000");  Sys.sleep(3)
      showGenomicRegion(igv, "chr18:59100000-59110000");  Sys.sleep(3)
      showGenomicRegion(igv, "chr17:59100000-59109000");  Sys.sleep(3)
      Sys.sleep(1)

      } # if interactive

} # test_displayUCSCBedGraphQuantitativeTrack
#------------------------------------------------------------------------------------------------------------------------
# TODO (31 mar 2019): temporarily disabled.  some latency problem with latest igv.js?
test_removeTracksByName <- function()
{
   message(sprintf("--- test_removeTracksByName"))
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

   #later(function() {
   #  trackNames <- getTrackNames(igv)
   #  checkTrue(track.name %in% trackNames)
     removeTracksByName(igv, track.name)
   #  checkTrue(!track.name %in% getTrackNames(igv))
   #  }, 0.5)
   Sys.sleep(3)

} # test_removeTracksByName
#------------------------------------------------------------------------------------------------------------------------
test_displayAlignment <- function()
{
   message(sprintf("--- test_displayAlignment"))

   bamFile <- system.file(package="igvR", "extdata", "tumor.bam")
   checkTrue(file.exists(bamFile))

   little.region <- GRanges(seqnames = "21", ranges = IRanges(10399760, 10401370))
   little.region <- GRanges(seqnames="21", ranges=IRanges(10400126, 10400326))
   showGenomicRegion(igv, "chr21:10,399,427-10,405,537")

   param <- ScanBamParam(which=little.region, what=scanBamWhat())
   x <- readGAlignments(bamFile, use.names=TRUE, param=param)
   #x <- readGAlignments(bamFile, use.names=TRUE)
   track <- GenomicAlignmentTrack("bam demo", x, visibilityWindow=2000000, trackHeight=500)  # 30000 default
   displayTrack(igv, track)
   print(getGenomicRegion(igv))

   loc <- "may not work immediately due to latency/concurrency complexities, especially acute with bam tracks"

} # test_displayAlignment
#------------------------------------------------------------------------------------------------------------------------
test_displayRemoteAlignment <- function()
{
   message(sprintf("--- test_displayRemoteAlignment"))

   setGenome(igv, "hg19")
   showGenomicRegion(igv, "GATA2")

   url <- "https://1000genomes.s3.amazonaws.com/phase3/data/HG02450/alignment/HG02450.mapped.ILLUMINA.bwa.ACB.low_coverage.20120522.bam"
   index <- "https://1000genomes.s3.amazonaws.com/phase3/data/HG02450/alignment/HG02450.mapped.ILLUMINA.bwa.ACB.low_coverage.20120522.bam.bai"

   track <- RemoteAlignmentTrack("Alignment", url, index, visibilityWindow=100000)
   displayTrack(igv, track)

} # test_displayRemoteAlignment
#------------------------------------------------------------------------------------------------------------------------
test_displayBedpeInteractions <- function()
{
   message(sprintf("--- test_displayBedpeInteractions"))

   setGenome(igv, "hg38")
   file.1 <- system.file(package="igvR", "extdata", "sixColumn-demo1.bedpe")
   checkTrue(file.exists(file.1))
   tbl.1 <- read.table(file.1, sep="\t", as.is=TRUE, header=TRUE)
   checkEquals(dim(tbl.1), c(32, 6))
      # bedpe tracks seem to ignore visibilityWindow, but no harm done by including it
   track <- BedpeInteractionsTrack("bedpe-6", tbl.1, color="red", visibilityWindow=10000000,
                                   trackHeight=200)

   shoulder <- 10000
   with(tbl.1, showGenomicRegion(igv, sprintf("%s:%d-%d", chrom1[1],
                                              min(start1)-shoulder, max(end2) + shoulder)))
   displayTrack(igv, track)

} # test_displayBedpeInteractions
#------------------------------------------------------------------------------------------------------------------------
test_displayGWAS.gwasFormat <- function()
{
   message(sprintf("--- test_displayGWAS.gwasFormat"))

   setGenome(igv, "hg19")
   #roi <- sprintf("%s:%d-%d", tbl.g2[1,3], min(tbl.g2[,4]), max(tbl.g2[,4]))
   #showGenomicRegion(igv, roi)
   gwas.file <- system.file(package="igvR", "extdata", "carolin.gwas")
   tbl.carolin <- read.table(gwas.file, header=TRUE, sep="\t", as.is=TRUE)

   track <- GWASTrack("gwas",
                      tbl.carolin,
                      chrom.col=3, pos.col=4, pval.col=10,
                      color=character(0),
                      visibilityWindow=100000, trackHeight=200)

   displayTrack(igv, track)

   track <- GWASTrack("gwas.colors",
                      tbl.carolin,
                      chrom.col=3, pos.col=4, pval.col=10,
                      color=rep(c("yellow", "green", "blue", "orange"), 6),
                      visibilityWindow=100000, trackHeight=200)

   displayTrack(igv, track)

} # test_displayGWAS.gwasFormat
#------------------------------------------------------------------------------------------------------------------------
test_displayGWAS.gwascatFormat <- function()
{
   message(sprintf("--- test_displayGWAS.gwascatFormat"))

      # saved a small subset of the april 2022 gwas catalog, just those with
      # alzheimer's disease as the mapped trait.
   data.dir <- system.file(package="igvR", "extdata", "gwas")
   file.exists(data.dir)
   file <- "alzheimerSubsetOfGWASCatatalog-29apr2022.RData"
   full.path <- file.path(data.dir, file)
   file.exists(full.path)

   tbl.gwascat.ad <- get(load(full.path))
   colnames(tbl.gwascat.ad)
   length(colnames(tbl.gwascat.ad))
   stopifnot(colnames(tbl.gwascat.ad)[c(1,2,33)] == c("seqnames", "start", "P.VALUE"))
     #  [1] "seqnames"                   "start"                      "end"
     #  [4] "width"                      "strand"                     "DATE.ADDED.TO.CATALOG"
     #  [7] "PUBMEDID"                   "FIRST.AUTHOR"               "DATE"
     # [10] "JOURNAL"                    "LINK"                       "STUDY"
     # [13] "DISEASE.TRAIT"              "INITIAL.SAMPLE.SIZE"        "REPLICATION.SAMPLE.SIZE"
     # [16] "REGION"                     "CHR_ID"                     "CHR_POS"
     # [19] "REPORTED.GENE.S."           "MAPPED_GENE"                "UPSTREAM_GENE_ID"
     # [22] "DOWNSTREAM_GENE_ID"         "SNP_GENE_IDS"               "UPSTREAM_GENE_DISTANCE"
     # [25] "DOWNSTREAM_GENE_DISTANCE"   "STRONGEST.SNP.RISK.ALLELE"  "SNPS"
     # [28] "MERGED"                     "SNP_ID_CURRENT"             "CONTEXT"
     # [31] "INTERGENIC"                 "RISK.ALLELE.FREQUENCY"      "P.VALUE"
     # [34] "PVALUE_MLOG"                "P.VALUE..TEXT."             "OR.or.BETA"
     # [37] "X95..CI..TEXT."             "PLATFORM..SNPS.PASSING.QC." "CNV"
     # [40] "MAPPED_TRAIT"               "MAPPED_TRAIT_URI"           "STUDY.ACCESSION"
     # [43] "GENOTYPING.TECHNOLOGY"

   setGenome(igv, "hg38")
   track <- GWASTrack("gwas ad",
                      tbl.gwascat.ad,
                      chrom.col=1, pos.col=2, pval.col=33,
                      visibilityWindow=100000, trackHeight=200)

   displayTrack(igv, track)

} # test_displayGWAS.gwascatFormat
#------------------------------------------------------------------------------------------------------------------------
test_displayGWASUrlTrack <- function()
{
    message(sprintf("--- test_displayGWASUrlTrack"))
    setGenome(igv, "hg38")
    url <- "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz"
    track <- GWASUrlTrack("igv.js demo", url, chrom.col=12, pos.col=13, pval.col=28,
                          autoscale=FALSE, min=0, max=400)
    displayTrack(igv, track)

} # test_displayGWASUrlTrack
#------------------------------------------------------------------------------------------------------------------------

# https://github.com/paul-shannon/igvR/issues/19
test_issue19_gwas_bug <- function()
{
    message(sprintf("--- test_issue19_gwas_bug"))
    setBrowserWindowTitle(igv, "issue 19")
    setGenome(igv, "hg19")

    showGenomicRegion(igv, "chr5:88,479,257-89,160,424")

    file <- system.file(package="igvR", "extdata", "tbl.mef2cGWAS.variants.RData")
    checkTrue(file.exists(file))
    tbl.gwas <- get(load(file))
    dim(tbl.gwas) # 178 9
    checkEquals(colnames(tbl.gwas)[c(1,3,9)], c("CHR", "BP", "P"))
    track <- GWASTrack("GWAS", tbl.gwas, chrom.col=1, pos.col=3, pval.col=9)
    displayTrack(igv, track)

} # test_issue19_gwas_bug
#------------------------------------------------------------------------------------------------------------------------
test_displayGFF3Track <- function()
{
   message(sprintf("--- test_displayGFF3Track"))

   setGenome(igv, "hg38")
   showGenomicRegion(igv, "NDUFS2")

       #-----------------------------------------------------
       # first, a remote file, provided by igv on amazon s3
       #-----------------------------------------------------

   track <- GFF3Track(trackName="url gff3",
                      url="https://s3.amazonaws.com/igv.org.genomes/hg38/Homo_sapiens.GRCh38.94.chr.gff3.gz",
                      indexURL="https://s3.amazonaws.com/igv.org.genomes/hg38/Homo_sapiens.GRCh38.94.chr.gff3.gz.tbi",
                      trackColor="brown",
                      displayMode="EXPANDED", trackHeight=200, visibilityWindow=1000)

   displayTrack(igv, track)

       #-----------------------------------------------------
       # now a local version of that file; use a color table
       #-----------------------------------------------------

   file <- "GRCh38.94.NDUFS2.gff3"
   data.dir <- system.file(package="igvR", "extdata")
   full.path <- file.path(data.dir, file)
   checkTrue(file.exists(full.path))
   tbl.gff3 <- read.table(full.path, sep="\t", as.is=TRUE, nrow=-1, header=FALSE)
   dim(tbl.gff3)

   library(stringr)
   matches <- str_extract(tbl.gff3[,9], regex("biotype=.*?;"))
   deleters <- which(is.na(matches))
   if(length(deleters) > 0) matches <- matches[-deleters]
   length(matches)
   biotypes <- unique(sub("biotype=", "", sub(";", "", matches)))
   # printf("-- found these biotypes in the gff3 file: %s", paste(biotypes, collapse=","))
   color.table <- list(processed_transcript="blue",
                       protein_coding="darkgreen",
                       retained_intron="brown",
                       nonsense_mediated_decay="orange",
                       miRNA="darkred",
                       default="black")

   track <- GFF3Track(trackName="file gff3",
                      tbl.track <- tbl.gff3,
                      url=NA_character_,
                      indexURL=NA_character_,
                      trackColor="gray",
                      colorByAttribute="biotype",
                      colorTable=color.table,
                      displayMode="EXPANDED",
                      trackHeight=350,
                      visibilityWindow=10000)

   showGenomicRegion(igv, "chr1:161,175,999-161,244,327")
   displayTrack(igv, track)

} # test_displayGFF3Track
#------------------------------------------------------------------------------------------------------------------------
test_saveToSVG <- function()
{
   message(sprintf("--- test_saveToSVG"))
   setGenome(igv, "hg38")
   showGenomicRegion(igv, "GATA2")
   filename <- tempfile(fileext=".svg")
   saveToSVG(igv, filename)
   message(sprintf("file exists? %s", file.exists(filename)))
   message(sprintf("file size:   %d", file.size(filename)))
   checkTrue(file.exists(filename))
   checkTrue(file.size(filename) > 0)   # may still be being written

} # test_saveToSVG
#------------------------------------------------------------------------------------------------------------------------
# read a small slice of a small bigWig file, demonstrating display of a bigwig track
test_mouseBigWigFile <- function()
{
   setGenome(igv, "mm10")
   showGenomicRegion(igv, "TREM2")
   region <- getGenomicRegion(igv)
   shoulder <- 10000

   with(region, showGenomicRegion(igv, sprintf("%s:%d-%d", chrom, start-shoulder, end+shoulder)))
   gr.region <- with(region, GRanges(seqnames=chrom, ranges=IRanges(start-shoulder, end+shoulder)))
   bw.file <- system.file(package="igvR", "extdata", "mm10-sample.bw")
   gr.atac <- import(bw.file, which=gr.region)

   track <- GRangesQuantitativeTrack("microglial ATAC-seq", gr.atac, autoscale=TRUE)
   displayTrack(igv, track)

} # test_mouseBigWigFile
#------------------------------------------------------------------------------------------------------------------------
# from https://www.gencodegenes.org/human both gtf and gff3 formats
#  http://genometools.org/cgi-bin/gff3validator.cgi
test_genomeAnnotationTracks <- function()
{
    message(sprintf("--- test_genomeAnnotationTracks"))

    setGenome(igv, "hg38")
    showGenomicRegion(igv, "NDUFS2")
    zoomOut(igv)

    roi <- "chr1:161,179,008-161,225,076"
    file <- system.file(package="igvR", "extdata", "geneAnnotations", "gencode.v39.annotation.gff3.gz")
    checkTrue(file.exists(file))
    tbl.gff3 <- read.table(file)
    dim(tbl.gff3) # 3238846       9
    colnames(tbl.gff3) <- c("chrom", "source", "feature", "start", "end", "score", "strand", "frame")
    lapply(tbl.gff3, class)
    tbl <- subset(tbl.gff3, chrom=="chr1" & start >= 161179008 & end <= 161225076)
    dim(tbl)  # 1995 9
    track <- DataFrameAnnotationTrack("gff3", tbl[, c(1,4,5)], color="brown")
    displayTrack(igv, track)

} # test_genomeAnnotationTracks
#------------------------------------------------------------------------------------------------------------------------
# no longer supported, but maybe someday resurrected
notest_.writeMotifLogoImagesUpdateTrackNames <- function()
{
   message(sprintf("--- test_.writeMotifLogoImagesUpdateTrackNames"))
   require(MotifDb)
   require(seqLogo)
   tbl <- get(load(system.file(package="igvR", "extdata", "tbl.with.MotifDbNames.Rdata")))
   checkEquals(tbl$name,
               c("MotifDb::Hsapiens-HOCOMOCOv10-MEF2C_HUMAN.H10MO.C",
                 "MA0803.1",
                 "MotifDb::Hsapiens-jaspar2018-MEF2C-MA0497.1"))

   tbl.fixed <- igvR:::.writeMotifLogoImagesUpdateTrackNames(tbl, igvApp.uri="http://localhost:15000")
   checkEquals(dim(tbl), dim(tbl.fixed))
   checkEquals(tbl[, -4], tbl.fixed[, -4])
   checkEquals(tbl.fixed$name[2], "MA0803.1")
   checkEquals(grep("http://localhost:15000?/", tbl.fixed$name, fixed=TRUE), c(1, 3))

} # notest_.writeMotifLogoImagesUpdateTrackNames
#------------------------------------------------------------------------------------------------------------------------
explore_blockingTrackLoad <- function()
{
   print(0)
   setGenome(igv, "hg19")
   print(1)
   bamFile <- "~/github/igvR/vignettes/macs2/GSM749704_hg19_wgEncodeUwTfbsGm12878CtcfStdAlnRep1.bam"
   print(2)
   checkTrue(file.exists(bamFile))
   print(3)
   big.region <- GRanges(seqnames = "chr19", ranges = IRanges(10000000,
                                                              10900000))
   print(4)

   param <- ScanBamParam(which=big.region, what=scanBamWhat())
   print(5)

   x <- readGAlignments(bamFile, use.names=TRUE, param=param)
   print(6)
   region.start <- start(range(ranges(x)))
   print(7)
   region.end   <- end(range(ranges(x)))
   print(8)

   showGenomicRegion(igv, sprintf("chr19:%d-%d", region.start, region.end))
   print(9)

   width <- round(width(range(ranges(x))) * 1.1)
   print(10)
   track <- GenomicAlignmentTrack("bam demo", x, visibilityWindow=width, trackHeight=500)  # 30000 default
   print(11)
   displayTrack(igv, track)
   print(12)
   print(getGenomicRegion(igv))
   print(13)
   browser()
   xyz <- 99
   print(14)

   #loc <- "may not work immediately due to latency/concurrency complexities, especially acute with bam tracks"

   #while(is.character(loc)){
   #   loc <- getGenomicRegion(igv)
   #   }

   #broad.loc <- with(loc, sprintf("%s:%d-%d", chrom, start-45000, end+45000))
   #showGenomicRegion(igv, broad.loc)

} # explore_blockingTrackLoad
#------------------------------------------------------------------------------------------------------------------------
demo_addTrackClickFunction_proofOfConcept <- function()
{
   message(sprintf("--- demo_addTrackClickFunction_proofOfConcept"))

   if(BrowserViz::webBrowserAvailableForTesting()){
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
demo_addTrackClickFunction_displayMotifLogo <- function()
{
   message(sprintf("--- demo_addTrackClickFunction_displayMotifLogo"))

   if(BrowserViz::webBrowserAvailableForTesting()){
      checkTrue(ready(igv))
      setGenome(igv, "hg38")
         # enableMotifLogoPopups(igv, TRUE)  # no longer necesssary: always on
      new.region <- "chr5:88,882,214-88,884,364"
      showGenomicRegion(igv, new.region)
      base.loc <- 88883100
      element.names <- c("MotifDb::Hsapiens-HOCOMOCOv10-MEF2C_HUMAN.H10MO.C",
                         "MA0803.1",
                         "MotifDb::Hsapiens-jaspar2018-MEF2C-MA0497.1")
      tbl <- data.frame(chrom=rep("chr5", 3),
                        start=c(base.loc, base.loc+100, base.loc + 250),
                        end=c(base.loc + 50, base.loc+120, base.loc+290),
                        name=element.names,
                        stringsAsFactors=FALSE)
      track <- DataFrameAnnotationTrack("dataframeTest", tbl, color="darkGreen", displayMode="EXPANDED")
      displayTrack(igv, track)
      } # if webBrowserAvailableForTesting

} # demo_displaySimpleBedTrackDirect_displayMotifLogo
#------------------------------------------------------------------------------------------------------------------------
create.gwas.test.datasets <- function()
{
   if(!exists("igv"))
      igv <- start.igv("NDUFS2", "hg38")
   setGenomicRegion(igv, "chr1:161,172,691-161,241,018")
   roi <- getGenomicRegion(igv)
   require(RPostgreSQL)
   db <- dbConnect(PostgreSQL(), user="trena", password="trena", dbname="genereg2021", host="khaleesi")
   dbGetQuery(db, "select * from eqtls limit 3")
   query <- sprintf("select * from eqtls where chrom='%s' and hg38 > %d and hg38 < %d and study='%s'",
                    roi$chrom, roi$start, roi$end, "ampad-mayo")
   tbl.eqtl <- dbGetQuery(db, query)
   tbl.eqtl <- subset(tbl.eqtl, tissue=="cer" & pvalue < 0.05 & genesymbol=="NDUFS2")
   dim(tbl.eqtl) # 43 10
    #  [1] "chrom"      "hg19"       "hg38"       "rsid"       "pvalue"     "ensg"       "genesymbol"
    #  [8] "study"      "tissue"     "assay"
  save(tbl.eqtl, file="../extdata/gwas/ampad.eqtl.ndufs2.RData")

     #---------------------------
     # a subset of the gwas.cat
     #---------------------------

  gwas.cat <- get(load(system.file(package="igvR", "extdata", "gwas", "gwascat-31oct2021.RData")))
  ad.traits <- unique(grep("alzheimer", gwas.cat$MAPPED_TRAIT, ignore.case=TRUE, value=TRUE))
  x <- which(gwas.cat[, "MAPPED_TRAIT"]$MAPPED_TRAIT %in% ad.traits)
  gwas.cat.ad <- sort(gwas.cat[x,])
  length(gwas.cat.ad)  # 1187
  save(gwas.cat.ad, file="../extdata/gwas/gwas.cat.alzheimer.granges")

} # create.gwas.test.datasets
#------------------------------------------------------------------------------------------------------------------------
# if(BrowserViz::webBrowserAvailableForTesting())
#   runTests()
