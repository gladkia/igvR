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

