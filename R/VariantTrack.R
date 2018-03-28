setClassUnion("VCF.or.NULL", members=c("VCF", "NULL"))

.VariantTrack <- setClass("VariantTrack",
                                 contains="Track",
                                 slots=c(
                                    displayMode="character",
                                    vcf.obj="VCF.or.NULL",
                                    vcf.url="list",
                                    locationColor="character",
                                    homvarColor="character",
                                    hetvarColor="character",
                                    homrefColor="character"
                                    )
                                 )



#----------------------------------------------------------------------------------------------------
VariantTrack <- function(trackName,
                         vcf,
                         trackHeight=50,
                         locationColor="pink",
                         homvarColor="rgb(17,248,254)",   # ~turquoise
                         hetvarColor="rgb(34,12,253)",    # ~royalBlue
                         homrefColor="rgb(200,200,200)",  # ~lightGray
                         displayMode="EXPANDED",
                         searchable=FALSE
                         )
{

   printf("VariantTrack ctor")

      # the vcf parameter may be an actual Biocondcutor VCF instance
      # or it me be a url to a hosted vcf file on a (typically public) webserver.
      # we determine this crucial difference first

   vcf.object.classes <- is(vcf)   # "is" reports multiple classes, from the class hierarchy
   vcf.obj <- NULL
   vcf.url <- list()

   if("VCF" %in% vcf.object.classes)
      vcf.obj <- vcf

   if("list" %in% vcf.object.classes) {
      if(all(sort(names(vcf) == c("data", "index"))))
         vcf.url <- vcf
      }

   if(is.null(vcf.obj) & length(vcf.url) == 0){
      stop("vcf argument neither a VCF nor a list or data & index urls")
      }

   obj <- .VariantTrack(Track(trackName=trackName,
                              trackType="variant",
                              color=locationColor,
                              fileFormat="vcf",
                              sourceType="file",
                              onScreenOrder=1,
                              height=trackHeight,
                              autoTrackHeight=FALSE,
                              minTrackHeight=50,
                              maxTrackHeight=500,
                              visibilityWindow=10e5),   # will be filled in by VariantTrack ctor if appropriate
                        displayMode=displayMode,
                        vcf.obj=vcf.obj,
                        vcf.url=vcf.url,
                        homvarColor=homvarColor,
                        hetvarColor=hetvarColor,
                        homrefColor=homrefColor)

   obj


} # VariantTrack
#----------------------------------------------------------------------------------------------------
setMethod("size", "VariantTrack",

    function(obj) {
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)
       })

#----------------------------------------------------------------------------------------------------
