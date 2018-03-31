#' @name QuantitativeTrack-class
#' @rdname QuantitativeTrack-class
#' @exportClass QuantitativeTrack


.QuantitativeTrack <- setClass("QuantitativeTrack",
                             contains="Track",
                             slots=c(
                                autoscale="logical",
                                min="numeric",
                                max="numeric")
                             )

#----------------------------------------------------------------------------------------------------
#' Constructor for QuantitativeTrack
#'
#' \code{QuantitativeTrack} creates an \code{IGV} track for genomic tracks in which a numerical value is
#' associated with each reported location.
#'
#' Detailed description will go here
#'
#' @name QuantitativeTrack
#' @rdname QuantitativeTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param quantitativeData  A polyvalent object, either a data.frame, GRanges, or UCSCBedGraphQuantitative object
#' @param fileFormat only "bedGraph" supported at present; wig and bigWig support soon.
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param sourceType only "file" supported at present ("gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API may come)
#' @param autoscale: Autoscale track to maximum value in view
#' @param min:  Sets the minimum value for the data (y-axis) scale. Usually zero.
#' @param max:  Sets the maximum value for the data (y-axis) scale. This value is ignored if autoscale is TRUE
#' @param visibilityWindow: Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A QuantitativeTrack object
#'
#' @export
#'
QuantitativeTrack <- function(trackName, quantitativeData, fileFormat, color,
                              sourceType="file", autoscale=TRUE, min=NA_real_, max=NA_real_,
                              visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   printf("QuantitativeTrack ctor")
   stopifnot(fileFormat %in% c("wig", "bigWig", "bedGraph"))

   obj <- .QuantitativeTrack(Track(trackType="quantitative",
                                   sourceType=sourceType,
                                   fileFormat=fileFormat,
                                   trackName=trackName,
                                   onScreenOrder=NA_integer_,
                                   color=color,
                                   height=50,
                                   autoTrackHeight=FALSE,
                                   minTrackHeight=50,
                                   maxTrackHeight=500,
                                   visibilityWindow=visibilityWindow),
                             min=min,
                             max=max
                             )
   obj


} # QuantitativeTrack
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
#' @export
#'

setMethod(size, "QuantitativeTrack",

    function(obj){
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)    # must be a remote url object, whose size we do not know
       })

#----------------------------------------------------------------------------------------------------
