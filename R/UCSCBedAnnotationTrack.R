.UCSCBedAnnotationTrack <- setClass("UCSCBedAnnotationTrack",
                                     contains="AnnotationTrack",
                                     slots=c(
                                         coreObject="UCSCData"
                                         )
                                     )
#----------------------------------------------------------------------------------------------------
UCSCBedAnnotationTrack <- function(trackName, annotation, color="darkGrey", displayMode="SQUISHED",
                                     expandedRowHeight=30, squishedRowHeight=15,
                                     maxRows=500, searchable=FALSE,
                            visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   printf("UCSCBedAnnotationTrack ctor")

   base.obj <- .AnnotationTrack(Track(trackType="annotation",
                                      sourceType="file",
                                      fileFormat="bed",
                                      displayMode=displayMode,
                                      trackName=trackName,
                                      onScreenOrder=NA_integer_,
                                      color=color,
                                      height=50,
                                      autoTrackHeight=FALSE,
                                      minTrackHeight=50,
                                      maxTrackHeight=500,
                                      visibilityWindow=visibilityWindow),
                                expandedRowHeight=expandedRowHeight,
                                squishedRowHeight=squishedRowHeight,
                                maxRows=maxRows,
                                searchable=searchable
                                )

   stopifnot(class(annotation) == "UCSCData")
   obj <- .UCSCBedAnnotationTrack(base.obj, coreObject=annotation)


} # AnnotationTrack
#----------------------------------------------------------------------------------------------------
setMethod("size", "UCSCBedAnnotationTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
