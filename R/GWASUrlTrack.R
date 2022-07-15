#' @name GWASTrack-class
#' @rdname GWASTrack-class
#' @exportClass GWASTrack

.GWASUrlTrack <- setClass("GWASUrlTrack",
                          contains="Track",
                          slots=c(
                              url="character",
                              chrom.col="numeric",
                              pos.col="numeric",
                              pval.col="numeric")
                          )



#----------------------------------------------------------------------------------------------------
#' Constructor for GWASUrlTrack
#'
#' \code{GWASUrlTrack} creates an \code{IGV} manhattan track GWAS data
#'
#' @name GWASUrlTrack
#' @rdname GWASUrlTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param table data.frame of 6 or more columns
#' @param chrom.col numeric, the column number of the chromosome column
#' @param pos.col numeric, the column number of the position column
#' @param pval.col numeric, the column number of the GWAS pvalue colum
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param displayMode  "COLLAPSED", "SQUISHED" or "EXPANDED".  Spelling and case must be precise.
#' @param color A css color name (e.g., "red" or "#FF0000"
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A GWASUrlTrack object
#'
#' @examples
#'
#'     #----------------------------
#'     #  first, from a local file
#'     #----------------------------
#'
#'   file <- system.file(package="igvR", "extdata", "tbl.mef2cGWAS.variants.RData")
#'   tbl.gwas <- get(load(file))
#'   dim(tbl.gwas)  #  32 6
#'   track <- GWASUrlTrack("GWAS from url",
#'                         "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz",
#'                          chrom.col=12, pos.col=13, pval.col=28)
#'
#'
#'     #------------------------------------------
#'     #  show the relevant portion of the genome
#'     #------------------------------------------
#'
#'   shoulder <- 10000
#'   roi <- with(tbl.gwas, sprintf("%s:%d-%d", CHR[1], min(BP)-shoulder, max(BP) + shoulder))
#'   # showGenomicRegion(igv, roi)
#'
#'   # displayTrack(igv, track)
#'
#' @export
#'

GWASUrlTrack <- function(trackName,
                         url,
                         chrom.col,
                         pos.col,
                         pval.col,
                         color="darkBlue",
                         trackHeight=50,
                         visibilityWindow=100000
                         )
{

    stopifnot(is.character(url))
    stopifnot(grepl("http", url))
    obj <- .GWASUrlTrack(Track(trackName=trackName,
                              trackType="variant",
                              color=color,
                              fileFormat="gwas",
                              sourceType="url",
                              onScreenOrder=1,
                              height=trackHeight,
                              autoTrackHeight=FALSE,
                              minTrackHeight=50,
                              maxTrackHeight=500,
                              visibilityWindow=visibilityWindow),
                         url=url,
                         chrom.col=chrom.col,
                         pos.col=pos.col,
                         pval.col=pval.col)

    obj@trackType <- "gwas"
    obj@fileFormat <- "gwas"
    obj

} # GWASUrlTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the GWASUrlTrack
#'
#' @param obj An object of class GWASUrlTrack
#' @return The number of elements
#'
#' @export
#'
setMethod("trackSize", "GWASUrlTrack",

    function(obj) {
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)
       })

#----------------------------------------------------------------------------------------------------
