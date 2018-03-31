#' @importFrom methods new
#' @import BiocGenerics
#'
#' @name Track-class
#' @rdname Track-class
#' @exportClass Track

.Track <- setClass ("Track",
                    slots = c(trackType="character",
                              sourceType="character",
                              fileFormat="character",
                              trackName="character",
                              onScreenOrder="numeric",
                              color="character",
                              height="numeric",
                              autoTrackHeight="logical",
                              minTrackHeight="numeric",
                              maxTrackHeight="numeric",
                              visibilityWindow="numeric")
                    )

#----------------------------------------------------------------------------------------------------
setGeneric('getInfo', signature='obj', function (obj) standardGeneric ('getInfo'))
setGeneric('getSize', signature='obj', function (obj) standardGeneric ('getSize'))
#----------------------------------------------------------------------------------------------------
#' Constructor for Track
#'
#' @name Track
#' @rdname Track-class
#'
#' @references \url{https://github.com/igvteam/igv.js/wiki/Tracks}
#' @references \url{https://www.w3schools.com/cssref/css_colors.asp}
#'
#' @param trackType One of "annotation", "quantitative", "variant".
#' @param sourceType Only "file" is currently supported.
#' @param fileFormat One of "bed", "bedGraph", "vdf"
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param onScreenOrder Numeric, for explicit placement of track within the current set.
#' @param color A CSS color name (e.g., "red" or "#FF0000")
#' @param height  track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param autoTrackHeight   If true, then track height is adjusted dynamically, within the bounds set by minHeight and maxHeight, to accomdodate features in view
#' @param minTrackHeight   In pixels, minimum allowed
#' @param maxTrackHeight   In pixels, maximum allowed
#' @param visibilityWindow  Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return An object of class Track
#'
#' @export
#'
Track <- function(trackType, sourceType, fileFormat, trackName,
                  onScreenOrder, color, height, autoTrackHeight, minTrackHeight, maxTrackHeight, visibilityWindow)
{

   printf("Track ctor")

      # see https://github.com/igvteam/igv.js/wiki/Tracks
   stopifnot(trackType %in% c("annotation", "quantitative", "alignment", "variant"))
   stopifnot(sourceType %in% c("file", "gcs", "ga4gh"))
   stopifnot(fileFormat %in% c("bed",
                               "gff", "gff3", "gtf",
                               "wig", "bigWig", "bedGraph",
                               "bam",
                               "vcf",
                               "seg"))
   stopifnot(is.character(trackName) && nchar(trackName) > 0)
   #stopifnot(is.character(url) && grepl("https*:\\/\\/", url))

   obj <- .Track(trackType=trackType,
                 sourceType=sourceType,
                 fileFormat=fileFormat,
                 trackName=trackName,
                 onScreenOrder=onScreenOrder,
                 color=color,
                 height=height,
                 autoTrackHeight=autoTrackHeight,
                 minTrackHeight=minTrackHeight,
                 maxTrackHeight=maxTrackHeight,
                 visibilityWindow=visibilityWindow)

} # Track
#----------------------------------------------------------------------------------------------------
#' Get basic info about a track: its type, file format, source and S4 class name
#'
#' @rdname getInfo
#' @aliases getInfo
#'
#' @param obj An object of base class Track
#'
#' @return A list with four fiels
#'
#' @examples
#'   track <- Track(trackType="annotaion", sourceType="file", fileFormat="bed",
#'                  trackName="demoTrack", onScreenOrder=NA_integer_, color="red",
#'                  height=40, autoTrackHeight=FALSE, minTrackHeight=50, maxTrackHeight=200,
#'                  visibilityWindow=100000)
#' getInfo(track)
#'
#' @export

setMethod("getInfo", "Track",

    function(obj){
       list(trackType=obj@trackType, fileFormat=obj@fileFormat, source=obj@sourceType,
            class=as.character(class(obj)))
        })

#----------------------------------------------------------------------------------------------------
