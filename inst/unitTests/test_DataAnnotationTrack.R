library(RUnit)
library(IGV)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_constructor()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_constructor <- function()
{
   printf("--- test_DataAnnotationTrack, test_constructor")
   tbl <- data.frame(chrom=rep("chr5", 3),
                     start=c(5000000, 5000020, 5000040),
                     end=c(5000010, 5000030, 5000050),
                     name=c("a", "b", "c"),
                     score=runif(3),
                     strand=rep("*", 3),
                     stringsAsFactors=FALSE)

   dat <- DataAnnotationTrack(trackName, tbl)

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
