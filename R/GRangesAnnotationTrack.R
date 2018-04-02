#' @name GRangesAnnotationTrack-class
#' @rdname GRangesAnnotationTrack-class
#' @exportClass GRangesAnnotationTrack

.GRangesAnnotationTrack <- setClass("GRangesAnnotationTrack",
                                       contains="AnnotationTrack",
                                       slots=c(
                                          coreObject="GRanges"
                                          )
                                       )
#' Constructor for GRangesAnnotationTrack
#'
#' \code{GRangesAnnotationTrack} creates and \code{IGV} track for bed-like objects expressed as GRanges
#'
#' Detailed description goes here
#'
#' @name GRangesAnnotationTrack
#' @rdname GRangesAnnotationTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param annotationData  A GRanges object with optional name metadata column
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param displayMode "COLLAPSED", "SQUISHED" or "EXPANDED".  Spelling and case must be precise.
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param expandedRowHeight  Height of each row of features in "EXPANDED" mode.
#' @param squishedRowHeight  Height of each row of features in "SQUISHED" mode, for compact viewing.
#' @param maxRows of features to display
#' @param searchable  If TRUE, labels on annotation elements may be used in search
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A GRangesAnnotationTrack object
#'
#' @examples
#' base.loc <- 88883100
#' tbl <- data.frame(chrom=rep("chr5", 3),
#'                   start=c(base.loc, base.loc+100, base.loc + 250),
#'                   end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                   name=c("a", "b", "c"),
#'                   strand=rep("*", 3),
#'                   stringsAsFactors=FALSE)
#'
#' gr <- GRanges(tbl)
#' track <- GRangesAnnotationTrack("GRangesQTest", gr)
#'
#' @export
#'

#----------------------------------------------------------------------------------------------------
GRangesAnnotationTrack <- function(trackName, annotationData, color="darkGrey", displayMode="SQUISHED",
                                   trackHeight=50, expandedRowHeight=30, squishedRowHeight=15,
                                   maxRows=500, searchable=FALSE,
                                   visibilityWindow=100000)
{
   printf("GRangesAnnotationTrack ctor")

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

   stopifnot("GRanges" %in% is(annotationData))

   obj <- .GRangesAnnotationTrack(base.obj, coreObject=annotationData)

} # GRangesAnnotationTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the GRangesAnnotationTrack
#'
#' @param obj An object of class GRangesAnnotationTrack
#'
#' @return The number of elements
#'
#' @export
#'
setMethod("getSize", "GRangesAnnotationTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
