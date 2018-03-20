setClassUnion("VCF.or.NULL", members=c("VCF", "NULL"))

.VariantTrack <- setClass("VariantTrack",
                                 contains="Track",
                                 slots=c(
                                    vcf.obj="VCF.or.NULL",
                                    vcf.url="list"
                                    )
                                 )



#----------------------------------------------------------------------------------------------------
VariantTrack <- function(trackName,
                         vcf,
                         color="black",
                         displayMode="COLLAPSED",
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

   #url <- "http://temporaryFileAt.httpuv.webserver"
   #indexURL <- NA_character_

   obj <- .VariantTrack(Track(trackName=trackName,
                              trackType="variant",
                              displayMode=displayMode,
                              color=color,
                              fileFormat="vcf",
                              sourceType="file",
                              #url=NA_character_,        # will be filled in by VariantTrack ctor if appropriate
                              #indexURL=NA_character_
                              onScreenOrder=1,
                              height=100,
                              autoTrackHeight=FALSE,
                              minTrackHeight=50,
                              maxTrackHeight=500,
                              visibilityWindow=10e5),   # will be filled in by VariantTrack ctor if appropriate
                        vcf.obj=vcf.obj,
                        vcf.url=vcf.url)
   obj


} # VariantTrack
#----------------------------------------------------------------------------------------------------

