library(RUnit)
library(IGV)
library(VariantAnnotation)

#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_Track_baseClass_constructor()
   test_AnnotationTrack_baseClass_constructor()
   test_DataAnnotationTrack_constructor()
   test_VariantDataTrack_constructor()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_Track_baseClass_constructor <- function()
{
   printf("--- test_Track abstract base class _constructor")

   track <- IGV:::Track(trackType="annotation",
                        sourceType="file",
                        fileFormat="bed",
                        displayMode="SQUISHED",
                        trackName="testOnly",
                        url="http://xxx.bed",
                        indexURL="http://xxx.bed.idx",
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
test_AnnotationTrack_baseClass_constructor <- function()
{
   printf("--- test_AnnotationTrack abstract base class constructor")

   at <- IGV:::AnnotationTrack(trackName="annotationTest",
                               displayMode="SQUISHED",
                               color="red",
                               fileFormat="wig",
                               sourceType="file",
                               url="http://trena/hg38/genes.gff.gz",
                               indexURL="http://trena/hg38/genes.gff.gz.idx",
                               expandedRowHeight=30,
                               squishedRowHeight=15,
                               GFF.GTF.id.columnName="Name",
                               maxRows=500,
                               searchable=FALSE)

   checkTrue(all(c("Track", "AnnotationTrack") %in% is(at)))

} # test_AnnotationTrack_baseClass_constructor
#------------------------------------------------------------------------------------------------------------------------
test_DataAnnotationTrack_constructor <- function()
{
   printf("--- test_DataAnnotationTrack_constructor")
   tbl <- data.frame(chrom=rep("chr5", 3),
                     start=c(5000000, 5000020, 5000040),
                     end=c(5000010, 5000030, 5000050),
                     name=c("a", "b", "c"),
                     score=runif(3),
                     strand=rep("*", 3),
                     stringsAsFactors=FALSE)

   dat <- DataAnnotationTrack("dataframeTest", tbl)

} # test_DataAnnotationTrack_constructor
#------------------------------------------------------------------------------------------------------------------------
test_VariantDataTrack_constructor <- function()
{
   printf("--- test_VariantDataTrack_constructor")
   f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
   rng <- GRanges(seqnames="22", ranges=IRanges(start=c(50301422, 50989541),
                                                end=c(50312106, 51001328),
                                                names=c("gene_79087", "gene_644186")))
   vcf.sub <- readVcf(f, "hg19", param=rng)
   track <- VariantDataTrack("chr22-tiny", vcf.sub)
   #writeVcf(vcf.sub, "chr22-subSub.vcf")




} # test_VariantDataTrack_constructor
#------------------------------------------------------------------------------------------------------------------------
