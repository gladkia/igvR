#' @name DataFrameAnnotationTrack-class
#' @rdname DataFrameAnnotationTrack-class
#' @exportClass DataFrameAnnotationTrack

.DataFrameAnnotationTrack <- setClass("DataFrameAnnotationTrack",
                                     contains="AnnotationTrack",
                                     slots=c(
                                         coreObject="data.frame"
                                         )
                                     )
#----------------------------------------------------------------------------------------------------
#' Constructor for DataFrameAnnotationTrack
#'
#' \code{DataFrameAnnotationTrack} creates and \code{IGV} track for bed objects imported using \code{rtracklayer}
#'
#' Detailed description goes here
#'
#' @name DataFrameAnnotationTrack
#' @rdname DataFrameAnnotationTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param annotation  A base R \code{data.frame}
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param displayMode "COLLAPSED", "SQUISHED" or "EXPANDED".  Spelling and case must be precise.
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param expandedRowHeight  Height of each row of features in "EXPANDED" mode.
#' @param squishedRowHeight  Height of each row of features in "SQUISHED" mode, for compact viewing.
#' @param maxRows of features to display
#' @param searchable  If TRUE, labels on annotation elements may be used in search
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A DataFrameAnnotationTrack object
#'
#' @examples
#' base.loc <- 88883100
#' tbl <- data.frame(chrom=rep("chr5", 3),
#'                   start=c(base.loc, base.loc+100, base.loc + 250),
#'                   end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                   name=c("a", "b", "c"),
#'                   score=runif(3),
#'                   strand=rep("*", 3),
#'                   stringsAsFactors=FALSE)
#'
#' track <- DataFrameAnnotationTrack("dataframeTest", tbl)
#'
#' @export
#'

DataFrameAnnotationTrack <- function(trackName, annotation, color="darkGrey", displayMode="SQUISHED",
                                     trackHeight=50, expandedRowHeight=30, squishedRowHeight=15,
                                     maxRows=500, searchable=FALSE,
                            visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   printf("DataFrameAnnotationTrack ctor")

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

   stopifnot(class(annotation) == "data.frame")
   obj <- .DataFrameAnnotationTrack(base.obj, coreObject=annotation)

} # AnnotationTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the DataFrameAnnotationTrack
#'
#' @param obj An object of class UCSCBedAnnotationTrack
#' @return The number of elements
#'
#' @examples
#' base.loc <- 88883100
#' tbl <- data.frame(chrom=rep("chr5", 3),
#'                   start=c(base.loc, base.loc+100, base.loc + 250),
#'                   end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                   name=c("a", "b", "c"),
#'                   score=runif(3),
#'                   strand=rep("*", 3),
#'                   stringsAsFactors=FALSE)
#'
#' track <- DataFrameAnnotationTrack("dataframeTest", tbl)
#' getSize(track)
#'
#' @export
#'
setMethod("getSize", "DataFrameAnnotationTrack",

    function(obj){
       return(nrow(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
