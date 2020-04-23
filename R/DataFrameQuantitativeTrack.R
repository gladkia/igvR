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
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param autoscale Autoscale track to maximum value in view
#' @param min  Sets the minimum value for the data (y-axis) scale. Usually zero.
#' @param max  Sets the maximum value for the data (y-axis) scale. This value is ignored if autoscale is TRUE
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @seealso DataFrameAnnotationTrack
#' @seealso GRangesQuantitativeTrack
#' @seealso GRangesAnnotationTrack
#' @seealso DataFrameAnnotationTrack
#' @seealso DataFrameQuantitativeTrack
#' @seealso GRangesAnnotationTrack
#' @seealso GRangesQuantitativeTrack
#' @seealso GenomicAlignmentTrack
#' @seealso UCSCBedAnnotationTrack
#' @seealso UCSCBedGraphQuantitativeTrack
#' @seealso VariantTrack
#' @seealso igvAnnotationTrack
#'
#' @return A DataFrameQuantitativeTrack object
#'
#' @examples
#' base.loc <- 88883100
#' tbl <- data.frame(chrom=rep("chr5", 3),
#'                   start=c(base.loc, base.loc+100, base.loc + 250),
#'                   end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                   score=runif(3),
#'                   stringsAsFactors=FALSE)
#'
#' track <- DataFrameQuantitativeTrack("dataframeTest", tbl, autoscale=TRUE)
#'
#' @export
#'

DataFrameQuantitativeTrack <- function(trackName, quantitativeData, color="blue", trackHeight=50,
                                       autoscale, min=NA_real_, max=NA_real_, visibilityWindow=100000)
{
   stopifnot(ncol(quantitativeData) >= 4)

   stopifnot(is.character(quantitativeData[, 1]))
   stopifnot(is.numeric(quantitativeData[, 2]))
   stopifnot(is.numeric(quantitativeData[, 3]))
   stopifnot(is.numeric(quantitativeData[, 4]))

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

   stopifnot("data.frame" %in% is(quantitativeData))
   obj <- .DataFrameQuantitativeTrack(base.obj, coreObject=quantitativeData)


} # DataFrameQuantitativeTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the DataFrameQuantitativeTrack
#'
#' @param obj An object of class DataFrameQuantitativeTrack
#'
#' @return The number of elements
#'
#' @export
#'
setMethod("trackSize", "DataFrameQuantitativeTrack",

    function(obj){
       return(nrow(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
