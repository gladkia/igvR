library(RUnit)
library(IGV)
#------------------------------------------------------------------------------------------------------------------------
runTests <- function()
{
   test_Track_constructor()
   test_AnnotationTrack_constructor()

} # runTests
#------------------------------------------------------------------------------------------------------------------------
test_Track_constructor <- function()
{
   printf("--- test_Track abstract base class _constructor")

   track <- IGV:::Track(trackType="annotation",
                       sourceType="file",
                       fileFormat="bed",
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

} # test_Track_constructor
#------------------------------------------------------------------------------------------------------------------------
test_AnnotationTrack_constructor <- function()
{
   printf("--- test_AnnotationTrack abstract base class constructor")

   at <- IGV:::AnnotationTrack(trackName="annotationTest",
                               displayMode="SQUISHED",
                               color="red",
                               expandedRowHeight=30,
                               squishedRowHeight=15,
                               GFF.GTF.id.columnName="Name",
                               maxRows=500,
                               searchable=FALSE)

   checkTrue(all(c("Track", "AnnotationTrack") %in% is(at)))

} # test_constructor
#------------------------------------------------------------------------------------------------------------------------
