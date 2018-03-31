#' @name GRangesQuantitativeTrack-class
#' @rdname GRangesQuantitativeTrack-class
#' @exportClass GRangesQuantitativeTrack

.GRangesQuantitativeTrack <- setClass("GRangesQuantitativeTrack",
                                       contains="QuantitativeTrack",
                                       slots=c(
                                          coreObject="GRanges"
                                          )
                                       )
#' Constructor for GRangesQuantitativeTrack
#'
#' \code{GRangesQuantitativeTrack} creates and \code{IGV} track for bed objects imported using \code{rtracklayer}
#'
#' Detailed description goes here
#'
#' @name GRangesQuantitativeTrack
#' @rdname GRangesQuantitativeTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param quantitativeData  A GRanges object with (at least) a "score" metadata column
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param trackHeight  track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param autoscale  Autoscale track to maximum value in view
#' @param min   Sets the minimum value for the data (y-axis) scale. Usually zero.
#' @param max   Sets the maximum value for the data (y-axis) scale. This value is ignored if autoscale is TRUE
#' @param visibilityWindow  Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A GRangesQuantitativeTrack object
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
#' gr <- GRanges(tbl)
#' track <- GRangesQuantitativeTrack("GRangesQTest", gr)
#'
#' @export
#'

#----------------------------------------------------------------------------------------------------
GRangesQuantitativeTrack <- function(trackName, quantitativeData, color="blue", trackHeight=50,
                                     autoscale=TRUE, min=NA_real_, max=NA_real_, visibilityWindow=100000)
{
   printf("GRangesQuantitativeTrack ctor")

   base.obj <- .QuantitativeTrack(Track(trackType="quantitative",
                                        sourceType="file",
                                        fileFormat="bedGraph",
                                        trackName=trackName,
                                        onScreenOrder=NA_integer_,
                                        color=color,
                                        height=trackHeight,
                                        autoTrackHeight=FALSE,
                                        minTrackHeight=50,
                                        maxTrackHeight=500,
                                        visibilityWindow=visibilityWindow),
                                  autoscale=autoscale,
                                  min=min,
                                  max=max
                                  )

   stopifnot("GRanges" %in% is(quantitativeData))
   obj <- .GRangesQuantitativeTrack(base.obj, coreObject=quantitativeData)

} # GRangesQuantitativeTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the GRangesQuantitativeTrack
#'
#' @param obj An object of class GRangesQuantitativeTrack
#'
#' @return The number of elements
#'
#' @export
#'
setMethod("getSize", "GRangesQuantitativeTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
