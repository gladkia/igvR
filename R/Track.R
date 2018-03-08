.Track <- setClass ("Track",
                    slots = c(trackType="character",
                              sourceType="character",
                              fileFormat="character",
                              trackName="character",
                              url="character",
                              indexURL="character",
                              onScreenOrder="numeric",
                              color="character",
                              height="numeric",
                              autoTrackHeight="logical",
                              minTrackHeight="numeric",
                              maxTrackHeight="numeric",
                              visibilityWindow="numeric")
                    )

#----------------------------------------------------------------------------------------------------
Track <- function(trackType, sourceType, fileFormat, trackName, url, indexURL, onScreenOrder, color,
                  height, autoTrackHeight, minTrackHeight, maxTrackHeight, visibilityWindow)
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
   stopifnot(is.character(url) && grepl("https*:\\/\\/", url))

   obj <- .Track(trackType=trackType,
                 sourceType=sourceType,
                 fileFormat=fileFormat,
                 trackName=trackName,
                 url=url,
                 indexURL=indexURL,
                 onScreenOrder=onScreenOrder,
                 color=color,
                 height=height,
                 autoTrackHeight=autoTrackHeight,
                 minTrackHeight=minTrackHeight,
                 maxTrackHeight=maxTrackHeight,
                 visibilityWindow=visibilityWindow)

} # Track
#----------------------------------------------------------------------------------------------------

