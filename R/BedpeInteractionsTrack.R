#' @name BedpeInteractionsTrack-class
#' @rdname BedpeInteractionsTrack-class
#' @exportClass BedpeInteractionsTrack

.BedpeInteractionsTrack <- setClass("BedpeInteractionsTrack",
                                    contains="DataFrameAnnotationTrack",
                                    )



#----------------------------------------------------------------------------------------------------
#' Constructor for BedpeInteractionsTrack
#'
#' \code{BedpeInteractionsTrack} creates an \code{IGV} track for two-location annotations
#'
#' @name BedpeInteractionsTrack
#' @rdname BedpeInteractionsTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param color A css color name (e.g., "red" or "#FF0000"
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A BedpeInteractionsTrack object
#'
#' @examples
#'
#'     #----------------------------
#'     #  first, from a local file
#'     #----------------------------
#'
#'   file <- system.file(package="igvR", "extdata", "sixColumn-demo1.bedpe")
#'   tbl.bedpe <- read.table(file, sep="\t", as.is=TRUE, header=TRUE)
#'   dim(tbl.1)  #  32 6
#'   track <- BedpeInteractionsTrack("bedpe-6", tbl.1)
#'
#'     #------------------------------------------
#'     #  show the relevant portion of the genome
#'     #------------------------------------------
#'
#'   shoulder <- 10000
#'   roi <- with(tbl, sprintf("%s:%d-%d", chrom1[1], min(start1)-shoulder, max(end2) + shoulder)))
#'   showGenomicRegion(igv, roi)
#'
#'   displayTrack(igv, track)
#
#     #-------------------------------------------
#     # now try a url track: not yet implemented
#     #------------------------------------------
#
# data.url <- sprintf("%s/%s", "https://s3.amazonaws.com/1000genomes/release/20130502",
#                                "ALL.wgs.phase3_shapeit2_mvncall_integrated_v5b.20130502.sites.vcf.gz")
# index.url <- sprintf("%s.tbi", data.url)
# url <- list(data=data.url, index=index.url)
#
# track.url <- VariantTrack("1kg", url)
#
#'
#' @export
#'

BedpeInteractionsTrack <- function(trackName, table, color="darkBlue",
                                   trackHeight=50,
                                   displayMode="EXPANDED",
                                   visibilityWindow=100000
                                   )
{

    obj <- .BedpeInteractionsTrack(
                DataFrameAnnotationTrack(trackName, table, color=color,
                                         displayMode=displayMode, trackHeight=trackHeight,
                                         visibilityWindow=visibilityWindow)
                                         )


    obj@trackType <- "pairedEndAnnotation"
    obj@fileFormat <- "bedpe"

    obj

} # BedpeInteractionsTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the BedpeInteractionsTrack
#'
#' @param obj An object of class BedpeInteractionsTrack
#' @return The number of elements
#'
#' @export
#'
setMethod("trackSize", "BedpeInteractionsTrack",

    function(obj) {
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)
       })

#----------------------------------------------------------------------------------------------------
