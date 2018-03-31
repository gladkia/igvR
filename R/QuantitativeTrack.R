.QuantitativeTrack <- setClass("QuantitativeTrack",
                             contains="Track",
                             slots=c(
                                autoscale="logical",
                                min="numeric",
                                max="numeric")
                             )

#----------------------------------------------------------------------------------------------------
QuantitativeTrack <- function(trackName, quantitativeData, fileFormat, color,
                              sourceType="file", autoscale=TRUE, min=NA_real_, max=NA_real_,
                              visibilityWindow=100000)
{
     # trackType: annotation, wig, alignment, variant, ga4gh.alignment, alignment.filter, variant.ga4gh
     # sourceType: "file", "gcs" for Google Cloud Storage, and "ga4gh" for the Global Alliance API
     # format: bed, gff, gff3, gtf, bedGraph, wig, vcf, ...

   printf("QuantitativeTrack ctor")
   stopifnot(fileFormat %in% c("wig", "bigWig", "bedGraph"))
   #stopifnot(quantitative.obj.class %in% c("data.frame", "UCSCData"))

   obj <- .QuantitativeTrack(Track(trackType="quantitative",
                                   sourceType=sourceType,
                                   fileFormat=fileFormat,
                                   #displayMode=displayMode,
                                   trackName=trackName,
                                   onScreenOrder=NA_integer_,
                                   color=color,
                                   height=50,
                                   autoTrackHeight=FALSE,
                                   minTrackHeight=50,
                                   maxTrackHeight=500,
                                   visibilityWindow=visibilityWindow),
                             #expandedRowHeight=expandedRowHeight,
                             #squishedRowHeight=squishedRowHeight,
                             #maxRows=maxRows,
                             #searchable=searchable
                             #classTypeOfSuppliedObject=quantitative.obj.class
                             min=min,
                             max=max
                             )
   obj


} # QuantitativeTrack
#----------------------------------------------------------------------------------------------------
setMethod(size, "QuantitativeTrack",

    function(obj){
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)    # must be a remote url object, whose size we do not know
       })

#----------------------------------------------------------------------------------------------------
