.Track <- setClass ("Track",
                    slots = c(trackType="character",
                              sourceType="character",
                              fileFormat="character",
                              trackName="character",
                              #url="character",
                              #indexed="logical",
                              #indexURL="character",
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
setGeneric('size', signature='obj', function (obj) standardGeneric ('size'))
#----------------------------------------------------------------------------------------------------
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
setMethod("getInfo", "Track",

    function(obj){
       list(trackType=obj@trackType, fileFormat=obj@fileFormat, source=obj@sourceType,
            class=as.character(class(obj)))
        })

#----------------------------------------------------------------------------------------------------
