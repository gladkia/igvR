#' @name VariantTrack-class
#' @rdname VariantTrack-class
#' @exportClass VariantTrack

setClassUnion("VCF.or.NULL", members=c("VCF", "NULL"))


.VariantTrack <- setClass("VariantTrack",
                                 contains="Track",
                                 slots=c(
                                    displayMode="character",
                                    vcf.obj="VCF.or.NULL",
                                    vcf.url="list",
                                    anchorColor="character",
                                    homvarColor="character",
                                    hetvarColor="character",
                                    homrefColor="character"
                                    )
                                 )



#----------------------------------------------------------------------------------------------------
#' Constructor for VariantTrack
#'
#' \code{VariantTrack} creates an \code{IGV} track for VCF (variant call format) objects, either local or at a remote url
#'
#' Detailed description goes here
#'
#' @name VariantTrack
#' @rdname VariantTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param vcf  A VCF object from the VariantAnnotation package, or a list(url=x, index=y) pointing to a vcf file
#' @param trackHeight: track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param anchorColor CSS color name (e.g., "red" or "#FF0000") for the "anchoring" graphical segment in the track
#' @param homvarColor  CSS color name for homozygous variant samples, rgb(17,248,254) by default (~turquoise)
#' @param hetvarColor CSS color name for heterzygous variant samples, rgb(34,12,253) by default (~royalBlue)
#' @param homrefColor CSS color names for homozygous reference samples, rgb(200,200,200) by default (~lightGray)
#' @param visibilityWindow: Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A VariantTrack object
#'
#' @examples
#'
#'     #----------------------------
#'     #  first, from a local file
#'     #----------------------------
#'
#' f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
#' roi <- GRanges(seqnames="22", ranges=IRanges(start=c(50301422, 50989541),
#'                                               end=c(50312106, 51001328),
#'                                               names=c("gene_79087", "gene_644186")))
#' vcf.sub <- readVcf(f, "hg19", param=roi)
#' track.local <- VariantTrack("chr22-tiny", vcf.sub)
#'
#'     #----------------------------
#'     # now try a url track
#'     #----------------------------
#'
#' data.url <- sprintf("%s/%s", "https://s3.amazonaws.com/1000genomes/release/20130502",
#'                                "ALL.wgs.phase3_shapeit2_mvncall_integrated_v5b.20130502.sites.vcf.gz")
#' index.url <- sprintf("%s.tbi", data.url)
#' url <- list(data=data.url, index=index.url)
#'
#' track.url <- VariantTrack("1kg", url)
#'
#' @export
#'

VariantTrack <- function(trackName,
                         vcf,
                         trackHeight=50,
                         anchorColor="pink",
                         homvarColor="rgb(17,248,254)",   # ~turquoise
                         hetvarColor="rgb(34,12,253)",    # ~royalBlue
                         homrefColor="rgb(200,200,200)",  # ~lightGray
                         displayMode="EXPANDED"
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
                              color=anchorColor,
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
#' Retrieve the size of the annotation
#'
#' @rdname getSize
#' @aliases getSize
#'
#' @param obj An object of class VariantTrack
#'
#' @return The number of elements
#'
#' @export
#'

setMethod("size", "VariantTrack",

    function(obj) {
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)
       })

#----------------------------------------------------------------------------------------------------
