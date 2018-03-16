.VariantTrack <- setClass("VariantTrack",
                                 contains="Track",
                                 slots=c(
                                    vcf.obj="VCF",
                                    vcf.url="character"
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
      # or it me be a url to a hosted vcf file on a (typically public) webserver
      # determine this crucial difference first

   vcf.object.classes <- is(vcf)   # "is" reports multiple classes, from the class hierarchy
   vcf.obj <- NULL
   vcf.url <- NA_character_

   if("VCF" %in% vcf.object.classes)
      vcf.obj <- vcf

   if("character" %in% vcf.object.classes)
      vcf.url <- vcf


   url <- "http://temporaryFileAt.httpuv.webserver"
   indexURL <- NA_character_

   obj <- .VariantTrack(Track(trackName=trackName,
                              trackType="variant",
                              displayMode=displayMode,
                              color=color,
                              fileFormat="vcf",
                              sourceType="file",
                              url=url,
                              onScreenOrder=1,
                              height=100,
                              autoTrackHeight=FALSE,
                              minTrackHeight=50,
                              maxTrackHeight=500,
                              visibilityWindow=10e5,
                              indexURL=indexURL),
                        vcf.obj=vcf.obj,
                        vcf.url=vcf.url)
   obj


} # VariantTrack
#----------------------------------------------------------------------------------------------------

