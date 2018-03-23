.UCSCBedGraphQuantitativeTrack <- setClass("UCSCBedGraphQuantitativeTrack",
                                       contains="QuantitativeTrack",
                                       slots=c(
                                          coreObject="UCSCData"
                                          )
                                       )
#----------------------------------------------------------------------------------------------------
UCSCBedGraphQuantitativeTrack <- function(trackName, quantitativeData, color="blue", autoscale=TRUE,
                                      min=NA_real_, max=NA_real_)
{
   printf("UCSCBedGraphQuantitativeTrack ctor")

   base.obj <- .QuantitativeTrack(Track(trackType="quantitative",
                                        sourceType="file",
                                        fileFormat="bedGraph",
                                        trackName=trackName,
                                        onScreenOrder=NA_integer_,
                                        color=color,
                                        height=50,
                                        autoTrackHeight=FALSE,
                                        minTrackHeight=50,
                                        maxTrackHeight=500,
                                        visibilityWindow=1000000),
                                  autoscale=autoscale,
                                  min=min,
                                  max=max
                                  )

   stopifnot(class(quantitativeData) == "UCSCData")
   obj <- .UCSCBedGraphQuantitativeTrack(base.obj, coreObject=quantitativeData)

} # UCSCBedGraphQuantitativeTrack
#----------------------------------------------------------------------------------------------------
setMethod("size", "UCSCBedGraphQuantitativeTrack",

    function(obj){
       return(length(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
