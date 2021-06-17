#' @name RemoteAlignmentTrack-class
#' @rdname RemoteAlignmentTrack-class
#' @exportClass RemoteAlignmentTrack
#'
#'


.RemoteAlignmentTrack <- setClass("RemoteAlignmentTrack",
                                 contains="Track",
                                 slots=c(
                                    bamUrl="character",
                                    bamIndex = "character"
                                    ))



#----------------------------------------------------------------------------------------------------
#' Constructor for RemoteAlignmentTrack
#'
#' \code{RemoteAlignmentTrack} creates an \code{IGV} track for remote bam files
#'
#' Detailed description goes here
#'
#' @name RemoteAlignmentTrack
#' @rdname RemoteAlignmentTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param bamUrl  The URL of a bam file
#' @param bamIndex  The URL of a bam index file. Defaults to <bamUrl>.bai
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param color A character string, either a reconized color ("red") or a hex string ("#FF8532")
#'
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A RemoteAlignmentTrack object
#'
#'
#' @export
#'
#----------------------------------------------------------------------------------------------------
RemoteAlignmentTrack <- function(trackName,
                                 bamUrl,
                                 bamIndex = NULL,
                                 trackHeight=50,
                                 visibilityWindow=30000,
                                 color="gray"
                                 )
{
   obj <- .RemoteAlignmentTrack(Track(trackName=trackName,
                                      trackType="remoteAlignment",
                                      fileFormat="bam",
                                      sourceType="url",
                                      color=color,
                                      onScreenOrder=1,
                                      height=trackHeight,
                                      autoTrackHeight=FALSE,
                                      minTrackHeight=50,
                                      maxTrackHeight=500,
                                      visibilityWindow=visibilityWindow),
                                bamUrl=bamUrl, bamIndex=bamIndex)

   obj


} # GenomicAlignmentTrack
