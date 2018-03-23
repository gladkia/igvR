.DataFrameQuantitativeTrack <- setClass("DataFrameQuantitativeTrack",
                                     contains="QuantitativeTrack",
                                     slots=c(
                                         coreObject="data.frame"
                                         )
                                     )
#----------------------------------------------------------------------------------------------------
DataFrameQuantitativeTrack <- function(trackName, quantitativeData, color="blue", autoscale=TRUE,
                                       min=NA_real_, max=NA_real_)
{
   printf("DataFrameQuantitativeTrack ctor")

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

   stopifnot(class(quantitativeData) == "data.frame")
   obj <- .DataFrameQuantitativeTrack(base.obj, coreObject=quantitativeData)


} # DataFrameQuantitativeTrack
#----------------------------------------------------------------------------------------------------
setMethod("size", "DataFrameQuantitativeTrack",

    function(obj){
       return(nrow(obj@coreObject))
       })

#----------------------------------------------------------------------------------------------------
