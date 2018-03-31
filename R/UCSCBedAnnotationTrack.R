#' @name UCSCBedAnnotationTrack-class
#' @rdname UCSCBedAnnotationTrack-class
#' @exportClass UCSCBedAnnotationTrack

.UCSCBedAnnotationTrack <- setClass("UCSCBedAnnotationTrack",
                                     contains="AnnotationTrack",
                                     slots=c(
                                         coreObject="UCSCData"
                                         )
                                     )
#----------------------------------------------------------------------------------------------------
#' Constructor for UCSCBedAnnotationTrack
#'
#' \code{UCSCBedAnnotationTrack} creates and \code{IGV} track for bed objects imported using \code{rtracklayer}
#'
#' Detailed description goes here
#'
#' @name UCSCBedAnnotationTrack
#' @rdname UCSCBedAnnotationTrack-class
#'
#' @name AnnotationTrack
#' @rdname AnnotationTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param annotation  A UCSCData object imported by \code{rtracklayer}
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param displayMode: "COLLAPSED", "SQUISHED" or "EXPANDED".  Spelling and case must be precise.
#' @param trackHeight: track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param expandedRowHeight:  Height of each row of features in "EXPANDED" mode.
#' @param squishedRowHeight:  Height of each row of features in "SQUISHED" mode, for compact viewing.
#' @param maxRows: of features to display
#' @param searchable:  If TRUE, labels on annotation elements may be used in search
#' @param visibilityWindow: Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A UCSCBedAnnotationTrack object
#'
#' @examples
#' bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
#' gr.bed <- import(bed.filepath)
#' track <- UCSCBedAnnotationTrack("UCSC bed", gr.bed,  color="blue", displayMode="SQUISHED")
#'
#' @export
#'

UCSCBedAnnotationTrack <- function(trackName, annotation, color="darkGrey", displayMode="SQUISHED",
                                   trackHeight=50,expandedRowHeight=30, squishedRowHeight=15,
                                   maxRows=500, searchable=FALSE, visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   printf("UCSCBedAnnotationTrack ctor")

   base.obj <- .AnnotationTrack(Track(trackType="annotation",
                                      sourceType="file",
                                      fileFormat="bed",
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
                                searchable=searchable
                                )

   stopifnot(class(annotation) == "UCSCData")
   obj <- .UCSCBedAnnotationTrack(base.obj, coreObject=annotation)


} # AnnotationTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the annotation
#'
#' @rdname getSize
#' @aliases getSize
#'
#' @param obj An object of class UCSCBedAnnotationTrack
#'
#' @return The number of elements
#'
#' @examples
#' bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
#' gr.bed <- import(bed.filepath)
#' track.1 <- UCSCBedAnnotationTrack("UCSC bed", gr.bed,  color="blue", displayMode="SQUISHED")
#' size(track.1)
#'
#' @export
#'
setMethod("size", "UCSCBedAnnotationTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
