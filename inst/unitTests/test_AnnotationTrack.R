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
   printf("--- test_AnnotationTrack, test_constructor")

   AnnotationTrack(trackName="annotationTest",
                   displayMode="SQUISHED",
                   color="red",
                   expandedRowHeight=30,
                   squishedRowHeight=15,
                   GFF.GTF.id.columnName="Name",
                   maxRows=500,
                   searchable=FALSE)

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
