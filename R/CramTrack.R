#' @name CramTrack-class
#' @rdname CramTrack-class
#' @exportClass CramTrack

.CramTrack <- setClass("CramTrack",
                       contains="Track",
                       slots=c(
                          cramUrl="character",
                          indexUrl="character"
                       ))

#----------------------------------------------------------------------------------------------------
#' Constructor for CramTrack
#'
#' \code{CramTrack} creates an \code{IGV} track for remote CRAM files
#'
#' @name CramTrack
#' @rdname CramTrack-class
#'
#' @param trackName  A character string, used as track label by igv.
#' @param cramUrl  The URL of a cram file.
#' @param indexUrl  The URL of a cram index file (.crai).
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000.
#' @param visibilityWindow Maximum window size in base pairs for which alignments are displayed. Defaults to 30 kb.
#' @param color A character string, either a recognized color ("red") or a hex string.
#'
#' @return A CramTrack object
#'
#' @export
CramTrack <- function(trackName,
                      cramUrl,
                      indexUrl,
                      trackHeight=50,
                      visibilityWindow=30000,
                      color="gray")
{
   # Note: trackType must be "alignment" for igv.js to use the alignment features
   # but we use "cram" for fileFormat to distinguish it in R dispatch if needed,
   # or rely on the class name.
   obj <- .CramTrack(Track(trackName=trackName,
                           trackType="alignment",
                           fileFormat="cram",
                           sourceType="url",
                           color=color,
                           onScreenOrder=1,
                           height=trackHeight,
                           autoTrackHeight=FALSE,
                           minTrackHeight=50,
                           maxTrackHeight=500,
                           visibilityWindow=visibilityWindow),
                     cramUrl=cramUrl,
                     indexUrl=indexUrl)
   obj
}
