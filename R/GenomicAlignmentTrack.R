#' @name GenomicAlignmentTrack-class
#' @rdname GenomicAlignmentTrack-class
#' @exportClass GenomicAlignmentTrack
#'
#' @import GenomicAlignments
#'


.GenomicAlignmentTrack <- setClass("GenomicAlignmentTrack",
                                 contains="Track",
                                 slots=c(
                                    alignment="GAlignments"
                                    ))



#----------------------------------------------------------------------------------------------------
#' Constructor for GenomicAlignmentTrack
#'
#' \code{GenomicAlignmentTrack} creates and \code{IGV} track for bed-like objects expressed as GRanges
#'
#' Detailed description goes here
#'
#' @name GenomicAlignmentTrack
#' @rdname GenomicAlignmentTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param alignment  A GAlignments object
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param color A character string, either a reconized color ("red") or a hex string ("#FF8532")
#'
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A GenomicAlignmentTrack object
#'
#' @examples
#'
#'   bamFile <- system.file(package="igvR", "extdata", "tumor.bam")
#'   which <- GRanges(seqnames = "21", ranges = IRanges(10400126, 10400326))
#'   param <- ScanBamParam(which=which, what = scanBamWhat())
#'   x <- readGAlignments(bamFile, use.names=TRUE, param=param)
#'   track <- GenomicAlignmentTrack("tumor", x)
#'
#' @export
#'
#----------------------------------------------------------------------------------------------------
GenomicAlignmentTrack <- function(trackName,
                                  alignment,
                                  trackHeight=50,
                                  visibilityWindow=30000,
                                  color="gray"
                                  )
{

   obj <- .GenomicAlignmentTrack(Track(trackName=trackName,
                                       trackType="genomicAlignment",
                                       fileFormat="bam",
                                       sourceType="file",
                                       color=color,
                                       onScreenOrder=1,
                                       height=trackHeight,
                                       autoTrackHeight=FALSE,
                                       minTrackHeight=50,
                                       maxTrackHeight=500,
                                       visibilityWindow=visibilityWindow),
                        alignment=alignment)

   obj


} # GenomicAlignmentTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the GenomicAlignmentTrack
#'
#' @param obj An object of class GenomicAlignmentTrack
#' @return The number of elements
#'
#' @export
#'
setMethod("trackSize", "GenomicAlignmentTrack",

    function(obj) {
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)
       })

#----------------------------------------------------------------------------------------------------
