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
   test_loadSimpleBedTrackDirect()
   test_loadVcfDirect()

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
test_loadVcfDirect <- function()
{
   printf("--- test_loadVcfDirect")
   setGenome(igv, "hg19")

   f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
   #f <- system.file("extdata", "chr7-sub.vcf.gz", package="VariantAnnotation")
   file.exists(f) # [1] TRUE
   vcf <- readVcf(f, "hg19")
      # get oriented around the contents of this vcf

   start <- 50586118
   end   <- 50633733
   rng <- GRanges(seqnames="22", ranges=IRanges(start=start, end=end))
                                               # names=c("gene_79087", "gene_644186")))
   vcf.sub <- readVcf(f, "hg19", param=rng)
   track <- VariantDataTrack("chr22-tiny", vcf.sub)
   displayTrack(igv, track)
   showGenomicRegion(igv, sprintf("chr22:%d-%d", start-1000, end+1000))

   #writeVcf(vcf.sub, "chr22-subSub.vcf")

   head(ranges(vcf))
      # IRanges object with 6 ranges and 0 metadata columns:
      #                   start       end     width
      #               <integer> <integer> <integer>
      #     rs7410291  50300078  50300078         1
      #   rs147922003  50300086  50300086         1
      #   rs114143073  50300101  50300101         1
      #   rs141778433  50300113  50300113         1
      #   rs182170314  50300166  50300166         1
      #   rs115145310  50300187  50300187         1

   geno(header(vcf))
      # DataFrame with 3 rows and 3 columns
      #         Number        Type                       Description
      #    <character> <character>                       <character>
      # GT           1      String                          Genotype
      # DS           1       Float Genotype dosage from MaCH/Thunder
      # GL           .       Float              Genotype Likelihoods
   dim(geno(vcf)$GT) # [1] 10376     5
   head(geno(vcf)$GT)
      #             HG00096 HG00097 HG00099 HG00100 HG00101
      # rs7410291   "0|0"   "0|0"   "1|0"   "0|0"   "0|0"
      # rs147922003 "0|0"   "0|0"   "0|0"   "0|0"   "0|0"
      # rs114143073 "0|0"   "0|0"   "0|0"   "0|0"   "0|0"
      # rs141778433 "0|0"   "0|0"   "0|0"   "0|0"   "0|0"
      # rs182170314 "0|0"   "0|0"   "0|0"   "0|0"   "0|0"
      # rs115145310 "0|0"   "0|0"   "0|0"   "0|0"   "0|0"
    head(geno(vcf)$DS)
      #             HG00096 HG00097 HG00099 HG00100 HG00101
      # rs7410291         0    0.00       1       0       0
      # rs147922003       0    0.00       0       0       0
      # rs114143073       0    0.00       0       0       0
      # rs141778433       0    0.00       0       0       0
      # rs182170314       0    0.05       0       0       0
      # rs115145310       0    0.00       0       0       0
     geno(vcf)$GL[1, 1] # [1]  0.00 -2.77 -5.00

      # igv.js display at chr7:55,206,977-55,217,618
   tbl.ov <- as.data.frame(findOverlaps(GRanges(seq="7", IRanges(55212130, 55212135)),  vcf))
   colnames(tbl.ov) <- c("roi", "vcf")

   vcf.filename <- "~/s/examples/vcf/ADNI.808_indiv.minGQ_21.pass.ADNI_ID.chr5.vcf.gz"
   vcf.tabix.filename <- sprintf("%s.tbi", vcf.filename)
   file.exists(vcf.filename)
   file.exists(vcf.tabix.filename)
   start.loc <- 88010000
   end.loc   <- 88012000
   mef2c.gr <- GRanges(5, IRanges(start.loc, end.loc))
   params <- ScanVcfParam(which=mef2c.gr)
   tabixFile <- TabixFile(vcf.filename)
   vcf <- readVcf(tabixFile, "hg19", params)

   vcf.filename <- "~/s/examples/vcf/SCH_11923_B01_GRM_WGS_2017-04-27_10.recalibrated_variants.vcf.gz"
   tabixFile <- TabixFile(vcf.filename)
   start.loc <- 88010000
   end.loc   <- 88012000
   mef2c.gr <- GRanges("10", IRanges(start.loc, end.loc))
   params <- ScanVcfParam(which=mef2c.gr)

   vcf <- readVcf(tabixFile, "hg19", params)



} # test_loadVcfDirect
#------------------------------------------------------------------------------------------------------------------------

