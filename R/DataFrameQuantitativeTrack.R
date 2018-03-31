#' @name DataFrameQuantitativeTrack-class
#' @rdname DataFrameQuantitativeTrack-class
#' @exportClass DataFrameQuantitativeTrack

.DataFrameQuantitativeTrack <- setClass("DataFrameQuantitativeTrack",
                                     contains="QuantitativeTrack",
                                     slots=c(
                                         coreObject="data.frame"
                                         )
                                     )
#----------------------------------------------------------------------------------------------------
#' Constructor for DataFrameQuantitativeTrack
#'
#' \code{DataFrameQuantitativeTrack} creates and \code{IGV} track for bed objects imported using \code{rtracklayer}
#'
#' Detailed description goes here
#'
#' @name DataFrameQuantitativeTrack
#' @rdname DataFrameQuantitativeTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param quantitativeData  A base R \code{data.frame}
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param trackHeight: track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param autoscale: Autoscale track to maximum value in view
#' @param min:  Sets the minimum value for the data (y-axis) scale. Usually zero.
#' @param max:  Sets the maximum value for the data (y-axis) scale. This value is ignored if autoscale is TRUE
#' @param visibilityWindow: Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A DataFrameQuantitativeTrack object
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
#' track <- DataFrameQuantitativeTrack("dataframeTest", tbl)
#'
#' @export
#'


DataFrameQuantitativeTrack <- function(trackName, quantitativeData, color="blue", trackHeight=50,
                                       autoscale=TRUE, min=NA_real_, max=NA_real_, visibilityWindow=100000)
{
   printf("DataFrameQuantitativeTrack ctor")

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

   stopifnot(class(quantitativeData) == "data.frame")
   obj <- .DataFrameQuantitativeTrack(base.obj, coreObject=quantitativeData)


} # DataFrameQuantitativeTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the annotation
#'
#' @rdname getSize
#' @aliases getSize
#'
#' @param obj An object of class DataFrameQuantitativeTrack
#'
#' @return The number of elements
#'
#' @export
#'
setMethod("size", "DataFrameQuantitativeTrack",

    function(obj){
       return(nrow(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
