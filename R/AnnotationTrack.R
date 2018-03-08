.AnnotationTrack <- setClass("AnnotationTrack",
                             contains="Track",
                             slots=c(
                                displayMode="character",
                                expandedRowHeight="numeric",
                                squishedRowHeight="numeric",
                                nameField="character",
                                maxRows="numeric",
                                searchable="logical")
                             )

#----------------------------------------------------------------------------------------------------
AnnotationTrack <- function(trackName, displayMode, color,
                            fileFormat, sourceType,
                            url=NA_character_, indexURL=NA_character_,
                            expandedRowHeight=30, squishedRowHeight=15,
                            GFF.GTF.id.columnName="NAME", maxRows=500, searchable=FALSE)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, wig, vcf?

   printf("AnnotationTrack ctor")
   obj <- .AnnotationTrack(Track(trackType="annotation",
                                 sourceType=sourceType,
                                 fileFormat=fileFormat,
                                 trackName=trackName,
                                 url="http://xxx/yyy/tmp.bed",
                                 indexURL=NA_character_,
                                 onScreenOrder=NA_integer_,
                                 color=color,
                                 height=50,
                                 autoTrackHeight=FALSE,
                                 minTrackHeight=50,
                                 maxTrackHeight=500,
                                 visibilityWindow=1000000))
   obj


} # AnnotationTrack
#----------------------------------------------------------------------------------------------------
