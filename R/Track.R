.Track <- setClass ("Track",
                    slots = c(trackType="character",
                              sourceType="character",
                              fileFormat="character",
                              trackName="character",
                              displayMode="character",
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
setGeneric('getType', signature='obj', function (obj) standardGeneric ('getType'))
setGeneric('size', signature='obj', function (obj) standardGeneric ('size'))
#----------------------------------------------------------------------------------------------------
Track <- function(trackType, sourceType, fileFormat, trackName, displayMode, # url, indexURL,
                  onScreenOrder, color, height, autoTrackHeight, minTrackHeight, maxTrackHeight, visibilityWindow)
{

   printf("Track ctor")

      # see https://github.com/igvteam/igv.js/wiki/Tracks
   stopifnot(trackType %in% c("annotation", "wig", "alignment", "variant"))
   stopifnot(sourceType %in% c("file", "gcs", "ga4gh"))
   stopifnot(fileFormat %in% c("bed",
                               "gff", "gff3", "gtf",
                               "wig", "bigWig", "bedGraph",
                               "bam",
                               "vcf",
                               "seg"))
   stopifnot(is.character(trackName) && nchar(trackName) > 0)
   #stopifnot(is.character(url) && grepl("https*:\\/\\/", url))
   stopifnot(displayMode %in% c("COLLAPSED", "SQUISHED", "EXPANDED"))

   obj <- .Track(trackType=trackType,
                 sourceType=sourceType,
                 fileFormat=fileFormat,
                 trackName=trackName,
                 displayMode=displayMode,
                 #url=url,
                 #indexed=FALSE,
                 #indexURL=indexURL,
                 onScreenOrder=onScreenOrder,
                 color=color,
                 height=height,
                 autoTrackHeight=autoTrackHeight,
                 minTrackHeight=minTrackHeight,
                 maxTrackHeight=maxTrackHeight,
                 visibilityWindow=visibilityWindow)

} # Track
#----------------------------------------------------------------------------------------------------
setMethod("getType", "Track",

    function(obj){
       list(trackType=obj@trackType, fileFormat=obj@fileFormat, source=obj@sourceType,
            class=as.character(class(obj)))
        })

#----------------------------------------------------------------------------------------------------
