library(RUnit)
library(IGV)
library(VariantAnnotation)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_Track_baseClass_constructor()
   test_VariantTrack_constructor()
   test_AnnotationTrack_constructors()
   test_QuantitativeTrack_constructors()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_Track_baseClass_constructor <- function()
{
   printf("--- test_Track abstract base class _constructor")

   track <- IGV:::Track(trackType="annotation",
                        sourceType="file",
                        fileFormat="bed",
                        trackName="testOnly",
                        onScreenOrder=1,
                        color="red",
                        height=50,
                        autoTrackHeight=FALSE,
                        minTrackHeight=50,
                        maxTrackHeight=500,
                        visibilityWindow=1000000)

   checkTrue(is(track) == "Track")

} # test_Track_baseClass_constructor
#------------------------------------------------------------------------------------------------------------------------
# two kinds of VariantTrack:
#   1) constructed with an in-memory (local) VCF object
#   2) constructed with a remote url and indexURL
# test both of them here
test_VariantTrack_constructor <- function()
{
   printf("--- test_VariantTrack_constructor")
   f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")

      # read in a small VCF object shipped with the VariantAnnotation package

   roi <- GRanges(seqnames="22", ranges=IRanges(start=c(50301422, 50989541),
                                                end=c(50312106, 51001328),
                                                names=c("gene_79087", "gene_644186")))
   vcf.sub <- readVcf(f, "hg19", param=roi)
   track <- VariantTrack("chr22-tiny", vcf.sub)
   checkEquals(length(track@vcf.url), 0)
   checkEquals(track@vcf.obj, vcf.sub)

      #----------------------------
      # now try a url track
      #----------------------------

   data.url <- sprintf("%s/%s", "https://s3.amazonaws.com/1000genomes/release/20130502",
                                 "ALL.wgs.phase3_shapeit2_mvncall_integrated_v5b.20130502.sites.vcf.gz")
   index.url <- sprintf("%s.tbi", data.url)
   url <- list(data=data.url, index=index.url)

   track <- VariantTrack("1kg", url)
   checkEquals(length(track@vcf.url), 2)
   checkTrue(is.null(track@vcf.obj))

} # test_VariantTrack_constructor
#------------------------------------------------------------------------------------------------------------------------
# an annotation, roughly speaking, just a table of locations with one or more (perhaps many more) attributes.
# they are provided as files in one of four formats, all nicely supported by michael lawrence's rtracklayer
# package.  test them out here, along with a simple data.frame and a generic GRanges object
test_AnnotationTrack_constructors <- function()
{
   printf("--- test_AnnotationTrack_constructors")

     #----------------------------------------------------------------------------------------
     # read a short 12-column (that is, maximally complete) bed format file from rtracklayer
     #----------------------------------------------------------------------------------------

   bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
   checkTrue(file.exists(bed.filepath))
   tbl.bed <- read.table(bed.filepath, sep="\t", as.is=TRUE, skip=2)
   colnames(tbl.bed) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
                          "thickStart", "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts")

   track.0 <- DataFrameAnnotationTrack("bed file", tbl.bed)
   checkTrue(all(c("DataFrameAnnotationTrack", "AnnotationTrack", "Track") %in% is(track.0)))
   checkEquals(size(track.0), 5)

     #-------------------------------------------------------------------
     # a UCSC BED format object, that is, a GRanges subtype "UCSCData"
     #-------------------------------------------------------------------

   gr.bed <- import(bed.filepath)
   checkTrue(class(gr.bed) == "UCSCData")   # UCSC BED format
   track.1 <- UCSCBedAnnotationTrack("UCSC bed", gr.bed,  color="blue", displayMode="SQUISHED")
   checkTrue(all(c("UCSCBedAnnotationTrack", "AnnotationTrack", "Track") %in% is(track.1)))
   checkEquals(size(track.1), 5)


} # test_AnnotationTrack_constructors
#------------------------------------------------------------------------------------------------------------------------
test_QuantitativeTrack_constructors <- function()
{
   printf("--- test_QuantitativeTrack_constructors")

     #----------------------------------------------------------------------------------------
     # read a short 4-column bedGraph file from the rtracklayer test set
     #----------------------------------------------------------------------------------------

   bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
   checkTrue(file.exists(bedGraph.filepath))

      # one metadata line at the top, without leading comment character. skip it.
   tbl.bg <- read.table(bedGraph.filepath, sep="\t", as.is=TRUE, skip=1)
   colnames(tbl.bg) <- c("chrom", "chromStart", "chromEnd", "score")

   track.0 <- DataFrameQuantitativeTrack("bedGraph", tbl.bg)
   checkTrue(all(c("DataFrameQuantitativeTrack", "QuantitativeTrack", "Track") %in% is(track.0)))
   checkEquals(getInfo(track.0), list(trackType="quantitative",
                                      fileFormat="bedGraph",
                                      source="file",
                                      class="DataFrameQuantitativeTrack"))
    checkEquals(size(track.0), 9)

     #-------------------------------------------------------------------
     # a UCSC BED format object, that is, a GRanges subtype "UCSCData"
     #-------------------------------------------------------------------

   gr.bed <- import(bedGraph.filepath)
   checkTrue(class(gr.bed) == "UCSCData")   # UCSC BED format
   track.1 <- UCSCBedGraphQuantitativeTrack("UCSC bg", gr.bed,  color="blue")
   checkTrue(all(c("UCSCBedGraphQuantitativeTrack", "QuantitativeTrack", "Track") %in% is(track.1)))
   checkEquals(size(track.1), 9)
   checkEquals(getInfo(track.1), list(trackType="quantitative",
                                      fileFormat="bedGraph",
                                      source="file",
                                      class="UCSCBedGraphQuantitativeTrack"))

} # test_AnnotationTrack_constructor
#------------------------------------------------------------------------------------------------------------------------
