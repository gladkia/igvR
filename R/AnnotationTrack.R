setClassUnion("GRanges.or.dataframe.or.NULL", members=c("GRanges", "data.frame", "NULL"))

.AnnotationTrack <- setClass("AnnotationTrack",
                             contains="Track",
                             slots=c(
                                classTypeOfSuppliedObject="character",
                                displayMode="character",
                                expandedRowHeight="numeric",
                                squishedRowHeight="numeric",
                                nameField="character",
                                maxRows="numeric",
                                searchable="logical")
                             )

#----------------------------------------------------------------------------------------------------
AnnotationTrack <- function(trackName, annotation, fileFormat, color, displayMode="SQUISHED",
                            sourceType="file",
                            trackHeight=30,
                            expandedRowHeight=30, squishedRowHeight=15,
                            GFF.GTF.id.columnName="NAME", maxRows=500, searchable=FALSE,
                            visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   printf("AnnotationTrack ctor")
   stopifnot(fileFormat %in% c("bed", "gff", "gff3", "gtf"))
   annotation.obj.class <- class(annotation)
   stopifnot(annotation.obj.class %in% c("data.frame",
                                         "UCSCData"))

   obj <- .AnnotationTrack(Track(trackType="annotation",
                                 sourceType=sourceType,
                                 fileFormat=fileFormat,
                                 trackName=trackName,
                                 onScreenOrder=NA_integer_,
                                 color=color,
                                 height=trackHeight,
                                 autoTrackHeight=FALSE,
                                 minTrackHeight=50,
                                 maxTrackHeight=500,
                                 visibilityWindow=visibilityWindow),
                           displayMode=displayMode,
                           expandedRowHeight=expandedRowHeight,
                           squishedRowHeight=squishedRowHeight,
                           maxRows=maxRows,
                           searchable=searchable,
                           classTypeOfSuppliedObject=annotation.obj.class
                           )
   obj


} # AnnotationTrack
#----------------------------------------------------------------------------------------------------
