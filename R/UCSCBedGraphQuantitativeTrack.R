#' @name UCSCBedGraphQuantitativeTrack-class
#' @rdname UCSCBedGraphQuantitativeTrack-class
#' @exportClass UCSCBedGraphQuantitativeTrack


.UCSCBedGraphQuantitativeTrack <- setClass("UCSCBedGraphQuantitativeTrack",
                                       contains="QuantitativeTrack",
                                       slots=c(
                                          coreObject="UCSCData"
                                          )
                                       )

#' Constructor for UCSCBedGraphQuantitativeTrack
#'
#' \code{UCSCBedGraphQuantitativeTrack} creates an \code{IGV} track for bedGraph objects
#' imported with \code{rtracklayer}
#'
#' Detailed description goes here
#'
#' @name UCSCBedGraphQuantitativeTrack
#' @rdname UCSCBedGraphQuantitativeTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param quantitativeData  A GRanges object with (at least) a "score" metadata column
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param autoscale  Autoscale track to maximum value in view
#' @param min   Sets the minimum value for the data (y-axis) scale. Usually zero.
#' @param max   Sets the maximum value for the data (y-axis) scale. This value is ignored if autoscale is TRUE
#' @param visibilityWindow  Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A UCSCBedGraphQuantitativeTrack object
#'
#' @examples
#' bedGraph.filepath <- system.file(package = "rtracklayer", "tests", "test.bedGraph")
#' gr.bedGraph <- rtracklayer::import(bedGraph.filepath)
#' track <- UCSCBedGraphQuantitativeTrack("UCSCBedGraphTest", gr.bedGraph)
#'
#' @export
#'


#----------------------------------------------------------------------------------------------------
UCSCBedGraphQuantitativeTrack <- function(trackName, quantitativeData, color="blue", trackHeight=50,
                                          autoscale=TRUE, min=NA_real_, max=NA_real_, visibilityWindow=100000)
{
   base.obj <- .QuantitativeTrack(Track(trackType="quantitative",
                                        sourceType="file",
                                        fileFormat="bedGraph",
                                        trackName=trackName,
                                        onScreenOrder=NA_integer_,
                                        color=color,
                                        height=50,
                                        autoTrackHeight=FALSE,
                                        minTrackHeight=50,
                                        maxTrackHeight=500,
                                        visibilityWindow=visibilityWindow),
                                  autoscale=autoscale,
                                  min=min,
                                  max=max
                                  )

   stopifnot(class(quantitativeData) == "UCSCData")
   obj <- .UCSCBedGraphQuantitativeTrack(base.obj, coreObject=quantitativeData)

} # UCSCBedGraphQuantitativeTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the UCSCBedGraphQuantitativeTrack
#'
#' @param obj An object of class UCSCBedGraphQuantitativeTrack
#'
#' @return The number of elements
#'
#' @export
#'

setMethod("trackSize", "UCSCBedGraphQuantitativeTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
