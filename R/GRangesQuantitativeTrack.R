.GRangesQuantitativeTrack <- setClass("GRangesQuantitativeTrack",
                                       contains="QuantitativeTrack",
                                       slots=c(
                                          coreObject="GRanges"
                                          )
                                       )
#----------------------------------------------------------------------------------------------------
GRangesQuantitativeTrack <- function(trackName, quantitativeData, color="blue", trackHeight=50,
                                     autoscale=TRUE, min=NA_real_, max=NA_real_)
{
   printf("GRangesQuantitativeTrack ctor")

   base.obj <- .QuantitativeTrack(Track(trackType="quantitative",
                                        sourceType="file",
                                        fileFormat="bedGraph",
                                        trackName=trackName,
                                        onScreenOrder=NA_integer_,
                                        color=color,
                                        height=trackHeight,
                                        autoTrackHeight=FALSE,
                                        minTrackHeight=50,
                                        maxTrackHeight=500,
                                        visibilityWindow=1000000),
                                  autoscale=autoscale,
                                  min=min,
                                  max=max
                                  )

   stopifnot("GRanges" %in% is(quantitativeData))
   obj <- .GRangesQuantitativeTrack(base.obj, coreObject=quantitativeData)

} # GRangesQuantitativeTrack
#----------------------------------------------------------------------------------------------------
setMethod("size", "GRangesQuantitativeTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
