#' @name AnnotationTrack-class
#' @rdname AnnotationTrack-class
#' @exportClass AnnotationTrack

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
#' Constructor for AnnotationTrack
#'
#' @name AnnotationTrack
#' @rdname AnnotationTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param annotation An opague type, currently either a data.frame, GRanges, or UCSCBed object from rtracklayer.
#' @param fileFormat Only "bed" is currently supported.
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param displayMode  "COLLAPSED", "EXPANDED", or "SQUISHED"
#' @param sourceType Only "file" sources are currently supported.
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param expandedRowHeight  Height of each row of features in "EXPANDED" mode.
#' @param squishedRowHeight  Height of each row of features in "SQUISHED" mode, for compact viewing.
#' @param maxRows of features to display
#' @param searchable  If TRUE, labels on annotation elements may be used in search
#' @param visibilityWindow  Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return An AnnotationTrack object

AnnotationTrack <- function(trackName, annotation,
                            fileFormat=c("bed"),   # to be added:  "gff", "gff3", "gtf"
                            color, displayMode=c("SQUISHED", "COLLAPSED", "EXPANDED"),
                            sourceType="file",
                            trackHeight=30,
                            expandedRowHeight=30, squishedRowHeight=15,
                            maxRows=500, searchable=FALSE,
                            visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   annotation.obj.class <- class(annotation)
   stopifnot(annotation.obj.class %in% c("data.frame", "UCSCData"))

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
                           displayMode=match.arg(displayMode),
                           expandedRowHeight=expandedRowHeight,
                           squishedRowHeight=squishedRowHeight,
                           maxRows=maxRows,
                           searchable=searchable,
                           classTypeOfSuppliedObject=annotation.obj.class
                           )
   obj


} # AnnotationTrack
#----------------------------------------------------------------------------------------------------
