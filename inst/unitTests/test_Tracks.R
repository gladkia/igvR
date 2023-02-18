library(RUnit)
library(igvR)
library(VariantAnnotation)
#------------------------------------------------------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "C")   # for consistent sort order
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_Track_baseClass_constructor()
   test_VariantTrack_constructor()
   test_AnnotationTrack_constructors()
   test_QuantitativeTrack_constructors()
   test_AlignmentTrack_constructors()
   test_BedpeInteractionsTrack()
   test_GWASTrack()
   test_GWASUrlTrack()
   test_dataframe.GFF3Track()
   test_url.GFF3Track()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_Track_baseClass_constructor <- function()
{
   message(sprintf("--- test_Track abstract base class _constructor"))

   track <- igvR:::Track(trackType="annotation",
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
   message(sprintf("--- test_VariantTrack_constructor"))
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
   message(sprintf("--- test_AnnotationTrack_constructors"))

     #----------------------------------------------------------------------------------------
     # read a short 12-column (that is, maximally complete) bed format file from rtracklayer
     #----------------------------------------------------------------------------------------

   bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
   checkTrue(file.exists(bed.filepath))
   tbl.bed <- read.table(bed.filepath, sep="\t", as.is=TRUE, skip=2)
   colnames(tbl.bed) <- c("chrom", "chromStart", "chromEnd", "name", "score", "strand",
                          "thickStart", "thickEnd", "itemRgb", "blockCount", "blockSizes", "blockStarts")

   track.0 <- DataFrameAnnotationTrack("bed file", tbl.bed)
   checkTrue(all(c("DataFrameAnnotationTrack", "igvAnnotationTrack", "Track") %in% is(track.0)))
   checkEquals(trackSize(track.0), 5)
   checkEquals(trackInfo(track.0), list(trackType="annotation",
                                      fileFormat="bed",
                                      source="file",
                                      class="DataFrameAnnotationTrack"))

     #-------------------------------------------------------------------
     # ensure inadmissible strand value is caught
     #-------------------------------------------------------------------
   tbl.bed[1, "strand"] <- "*"
   track.0.badStrand <- DataFrameAnnotationTrack("bed file", tbl.bed)

     #-------------------------------------------------------------------
     # a UCSC BED format object, that is, a GRanges subtype "UCSCData"
     #-------------------------------------------------------------------

   gr.bed <- import(bed.filepath)
   checkTrue("UCSCData" %in% is(gr.bed))   # UCSC BED format
   track.1 <- UCSCBedAnnotationTrack("UCSC bed", gr.bed,  color="blue", displayMode="SQUISHED")
   checkTrue(all(c("UCSCBedAnnotationTrack", "igvAnnotationTrack", "Track") %in% is(track.1)))
   checkEquals(trackSize(track.1), 5)
   checkEquals(trackInfo(track.1), list(trackType="annotation",
                                      fileFormat="bed",
                                      source="file",
                                      class="UCSCBedAnnotationTrack"))

     #--------------------------------------------------------------------------------
     # now a generic GRanges, not necessarily UCSCData, just needs chrom, start, end
     #-------------------------------------------------------------------------------

   gr.simple <- GRanges(tbl.bed[, c("chrom", "chromStart", "chromEnd", "name")])
   track.2 <- GRangesAnnotationTrack("generic GRanges", gr.simple)
   checkTrue(all(c("GRangesAnnotationTrack", "igvAnnotationTrack", "Track") %in% is(track.2)))
   checkEquals(trackSize(track.2), 5)
   checkEquals(trackInfo(track.2), list(trackType="annotation",
                                      fileFormat="bed",
                                      source="file",
                                      class="GRangesAnnotationTrack"))

   gr.simpler <- GRanges(tbl.bed[, c("chrom", "chromStart", "chromEnd")])
   track.3 <- GRangesAnnotationTrack("generic GRanges", gr.simpler)
   checkTrue(all(c("GRangesAnnotationTrack", "igvAnnotationTrack", "Track") %in% is(track.3)))
   checkEquals(trackSize(track.3), 5)
   checkEquals(trackInfo(track.3), list(trackType="annotation",
                                      fileFormat="bed",
                                      source="file",
                                      class="GRangesAnnotationTrack"))


} # test_AnnotationTrack_constructors
#------------------------------------------------------------------------------------------------------------------------
test_QuantitativeTrack_constructors <- function()
{
   message(sprintf("--- test_QuantitativeTrack_constructors"))

     #----------------------------------------------------------------------------------------
     # read a short 4-column bedGraph file from the rtracklayer test set
     #----------------------------------------------------------------------------------------

   bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
   checkTrue(file.exists(bedGraph.filepath))

      # one metadata line at the top, without leading comment character. skip it.
   tbl.bg <- read.table(bedGraph.filepath, sep="\t", as.is=TRUE, skip=1)
   colnames(tbl.bg) <- c("chrom", "chromStart", "chromEnd", "score")

   track.0 <- DataFrameQuantitativeTrack("bedGraph", tbl.bg, autoscale=TRUE)
   checkTrue(all(c("DataFrameQuantitativeTrack", "QuantitativeTrack", "Track") %in% is(track.0)))
   checkEquals(trackInfo(track.0), list(trackType="quantitative",
                                      fileFormat="bedGraph",
                                      source="file",
                                      class="DataFrameQuantitativeTrack"))
    checkEquals(trackSize(track.0), 9)

     #-------------------------------------------------------------------
     # a UCSC BED format object, that is, a GRanges subtype "UCSCData"
     #-------------------------------------------------------------------

   gr.bed <- import(bedGraph.filepath)
   checkTrue("UCSCData" %in% is(gr.bed))   # UCSC BED format
   track.1 <- UCSCBedGraphQuantitativeTrack("UCSC bg", gr.bed,  color="blue", autoscale=TRUE)
   checkTrue(all(c("UCSCBedGraphQuantitativeTrack", "QuantitativeTrack", "Track") %in% is(track.1)))
   checkEquals(trackSize(track.1), 9)
   checkEquals(trackInfo(track.1), list(trackType="quantitative",
                                      fileFormat="bedGraph",
                                      source="file",
                                      class="UCSCBedGraphQuantitativeTrack"))


     #-------------------------------------------------------------------
     # a simple, hand-built GRanges
     #-------------------------------------------------------------------
    base.loc <- 88883100
    tbl <- data.frame(chrom=rep("chr5", 3),
                      start=c(base.loc, base.loc+100, base.loc + 250),
                      end=c(base.loc + 50, base.loc+120, base.loc+290),
                      name=c("a", "b", "c"),
                      score=runif(3),
                      strand=rep("*", 3),
                      stringsAsFactors=FALSE)

    gr <- GRanges(tbl)
    track <- GRangesQuantitativeTrack("GRangesQTest", gr, autoscale=TRUE)

     #-------------------------------------------------------------------
     # a simple, flawed, hand-built data.frame example: a 3 snp example
     # bedgraph is a 4-column format:
     #    chromA  chromStartA  chromEndA  dataValueA
     # https://genome.ucsc.edu/goldenpath/help/bedgraph.html
     #-------------------------------------------------------------------

   tbl.flawed <- data.frame(chrom=rep("chr7", 3),
                            start=c(base.loc, base.loc+100, base.loc+1000),
                            end=c(base.loc, base.loc+100, base.loc+1000),
                            name=c("rs1", "rs2", "rs3"),    # this is plausible but illegal
                            score=c(0.5, 1.3, 8.9),
                            stringsAsFactors=FALSE)
   checkException(track <- DataFrameQuantitativeTrack("broken", tbl.flawed, autoscale=TRUE), silent=TRUE)
   tbl.fixed <- tbl.flawed[, c(1,2,3,5)]
   track.fixed <- DataFrameQuantitativeTrack("fixed", tbl.fixed, autoscale=TRUE)
   checkTrue(all(c("DataFrameQuantitativeTrack", "QuantitativeTrack", "Track") %in% is(track.fixed)))


} # test_QuantitativeTrack_constructors
#------------------------------------------------------------------------------------------------------------------------
test_AlignmentTrack_constructors <- function()
{
   message(sprintf("--- test_AlignmentTrack_constructors"))

   bamFile <- system.file(package="igvR", "extdata", "psg1.bam")
   stopifnot(file.exists(bamFile))

   which <- GRanges(seqnames = "chr19", ranges = IRanges(42866464, 42879822))
   param <- ScanBamParam(which=which)

   x <- readGAlignments(bamFile, use.names=TRUE, param=param)
   track <- GenomicAlignmentTrack("DNAse", x)
   checkTrue(all(c("GenomicAlignmentTrack", "Track") %in% is(track)))

     # use more of the Track parameters

   track.2 <- GenomicAlignmentTrack("DNAse", x, visibilityWindow=100000)


} # test_AlignementTrack_constructors
#------------------------------------------------------------------------------------------------------------------------
test_BedpeInteractionsTrack <- function()
{
   message(sprintf("--- test_BedpeInteractionsTrack_constructors"))

   file.1 <- system.file(package="igvR", "extdata", "sixColumn-demo1.bedpe")
   checkTrue(file.exists(file.1))
   file.2 <- system.file(package="igvR", "extdata", "tenColumn-demo2.bedpe")
   checkTrue(file.exists(file.2))

   tbl.1 <- read.table(file.1, sep="\t", as.is=TRUE)
   checkEquals(dim(tbl.1), c(33, 6))

   track <- BedpeInteractionsTrack("bedpe-6", tbl.1)
   checkTrue(all(c("BedpeInteractionsTrack", "DataFrameAnnotationTrack") %in% is(track)))
   checkEquals(trackInfo(track),
               list(trackType="pairedEndAnnotation",
                    fileFormat="bedpe",
                    source="file",
                    class="BedpeInteractionsTrack"))

   tbl.2 <- read.table(file.2, sep="\t", as.is=TRUE)
   checkEquals(dim(tbl.2), c(2, 10))
   track <- BedpeInteractionsTrack("bedpe-10", tbl.2)

   checkEquals(trackInfo(track),
               list(trackType="pairedEndAnnotation",
                    fileFormat="bedpe",
                    source="file",
                    class="BedpeInteractionsTrack"))

   track <- BedpeInteractionsTrack("bedpe-6", tbl.1, color="red", visibilityWindow=10000000)

   checkEquals(trackInfo(track),
               list(trackType="pairedEndAnnotation",
                    fileFormat="bedpe",
                    source="file",
                    class="BedpeInteractionsTrack"))

     #--------------------------------------------------------------------------------------
     # todo: follow the VariantTrack strategy, support a url as well as a direct data.frame
     #--------------------------------------------------------------------------------------

   url <- "https://s3.amazonaws.com/igv.org.test/data/gm12878_loops.bedpe.gz"


} # test_BedpeInteractionsTrack
#------------------------------------------------------------------------------------------------------------------------
test_GWASTrack <- function()
{
   message(sprintf("--- test_GWASTrack_constructors"))

   file <- system.file(package="igvR", "extdata", "gwas-3k-clean.RData")
   checkTrue(file.exists(file))
   tbl.gwas <- get(load(file))
   dim(tbl.gwas)

   checkEquals(dim(tbl.gwas), c(3109, 34))
   track <- GWASTrack("GWAS", tbl.gwas, chrom.col=12, pos.col=13, pval.col=28)
   checkTrue(all(c("GWASTrack", "QuantitativeTrack", "Track") %in% is(track)))

   track <- GWASTrack("GWAS", tbl.gwas, chrom.col=12, pos.col=13, pval.col=28,
                      autoscale=FALSE, min=0, max=30, trackHeight=100)

      #----------------------------------------------------------
      # test out custom colors, must be empty or match the names
      # of the chromosomes.  for hg38
      #-----------------------------------------------------------

   checkEquals(length(unique(tbl.gwas$CHR_ID)), 23)
   library(randomcoloR)
   set.seed(17)
   randomColors <- as.list(distinctColorPalette(30))  # more than we need
   names(randomColors) <- as.character(seq_len(28))
   names(randomColors)[29:30] <- c("X", "Y")
   checkTrue(all(tbl.gwas$CHR_ID %in% names(randomColors)))
   track <- GWASTrack("GWAS", tbl.gwas, chrom.col=12, pos.col=13, pval.col=28,
                      autoscale=FALSE, min=0, max=30, trackHeight=100,
                      colorTable=randomColors)
   checkTrue(all(c("GWASTrack", "QuantitativeTrack", "Track") %in% is(track)))


} # test_GWASTrack
#------------------------------------------------------------------------------------------------------------------------
test_GWASUrlTrack <- function()
{
   message(sprintf("--- test_GWASUrlTrack_constructors"))

   url <- "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz"
   track <- GWASUrlTrack("GWAS", url, chrom.col=12, pos.col=13, pval.col=28,
                         autoscale=FALSE, min=0, max=30, trackHeight=100)
   checkTrue(all(c("GWASUrlTrack", "Track") %in% is(track)))


      #----------------------------------------------------------
      # test out custom colors, must be empty or match the number
      # of chromosomes.  for hg38, 24.
      # independent examination of the aws file confirms
      # confirmed that the standard chromosome names were used
      # with no leading "chr"
      #-----------------------------------------------------------

   colors <- as.list(c(rep(c("red", "green"), 11), "blue", "orange"))
   names(colors) <- c(as.character(1:22), "X", "Y")
   track <- GWASUrlTrack("GWAS", url, chrom.col=12, pos.col=13, pval.col=28,
                         autoscale=FALSE, min=0, max=30, trackHeight=100,
                         colorTable=colors)
   checkTrue(all(c("GWASUrlTrack", "QuantitativeTrack", "Track") %in% is(track)))

} # test_GWASUrlTrack
#------------------------------------------------------------------------------------------------------------------------
test_dataframe.GFF3Track <- function()
{
   message(sprintf("--- test_dataframe.GFF3Track_constructors"))

   tbl.gff3 <- read.table(system.file(package="igvR", "extdata", "GRCh38.94.NDUFS2.gff3"),
                        sep="\t", as.is=TRUE)
   checkEquals(ncol(tbl.gff3), 9)
      # standard column names not needed, but helpful if you wish to examine the data
   colnames(tbl.gff3) <- c("seqid", "source", "type", "start", "end", "score", "strand",
                           "phase", "attributes")
   colors <- list("antisense"="blueviolet",
                  "protein_coding"= "blue",
                  "retained_intron"= "rgb(0, 150, 150)",
                  "processed_transcript"= "purple",
                  "processed_pseudogene"= "#7fff00",
                  "unprocessed_pseudogene"= "#d2691e",
                  "default"= "black")
   track <- GFF3Track("dataframe gff3", tbl.gff3,
                      colorByAttribute="biotype", colorTable=colors,
                      url=NA_character_, indexURL=NA_character_,
                      displayMode="EXPANDED", trackHeight=200, visibilityWindow=100000)

   checkTrue(all(c("GFF3Track", "igvAnnotationTrack", "Track") %in% is(track)))

} # test_dataframe.GFF3Track
#------------------------------------------------------------------------------------------------------------------------
test_url.GFF3Track <- function()
{
   message(sprintf("--- test_url.GFF3Track_constructors"))

   track <- GFF3Track(trackName="url gff3",
                      url="https://s3.amazonaws.com/igv.org.genomes/hg38/Homo_sapiens.GRCh38.94.chr.gff3.gz",
                      indexURL="https://s3.amazonaws.com/igv.org.genomes/hg38/Homo_sapiens.GRCh38.94.chr.gff3.gz.tbi",
                      trackColor="red",
                      displayMode="EXPANDED", trackHeight=200, visibilityWindow=100000)

} # test_url.GFF3Track
#------------------------------------------------------------------------------------------------------------------------
if(!interactive())
   runTests()
