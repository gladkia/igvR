#' @name GWASTrack-class
#' @rdname GWASTrack-class
#' @exportClass GWASTrack

.GWASTrack <- setClass("GWASTrack",
                       contains="DataFrameAnnotationTrack",
                       slots=c(
                           chrom.col="numeric",
                           pos.col="numeric",
                           pval.col="numeric")
                       )



#----------------------------------------------------------------------------------------------------
#' Constructor for GWASTrack
#'
#' \code{GWASTrack} creates an \code{IGV} manhattan track GWAS data
#'
#' @name GWASTrack
#' @rdname GWASTrack-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param table data.frame of 6 or more columns
#' @param chrom.col numeric, the column number of the chromosome column
#' @param pos.col numeric, the column number of the position column
#' @param pval.col numeric, the column number of the GWAS pvalue colum
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param color A css color name (e.g., "red" or "#FF0000"
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A GWASTrack object
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
#'   track <- GWASTrack("GWAS", tbl.gwas, chrom.col=12, pos.col=13, pval.col=28)
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

GWASTrack <- function(trackName,
                      table,
                      chrom.col,
                      pos.col,
                      pval.col,
                      color="darkBlue",
                      trackHeight=50,
                      visibilityWindow=100000
                      )
{

    stopifnot(is.data.frame(table))
    #stopifnot(ncol(table) == 5)
    #stopifnot(colnames(table) == c("chrom", "start", "end", "name", "score"))
    obj <- .GWASTrack(DataFrameAnnotationTrack(trackName,
                                               table,
                                               color=color,
                                               displayMode="EXPANDED",
                                               trackHeight=trackHeight,
                                               visibilityWindow=visibilityWindow),
                      chrom.col=chrom.col,
                      pos.col=pos.col,
                      pval.col=pval.col)

    obj@trackType <- "gwas"
    obj@fileFormat <- "gwas"

    obj

} # GWASTrack
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the GWASTrack
#'
#' @param obj An object of class GWASTrack
#' @return The number of elements
#'
#' @export
#'
setMethod("trackSize", "GWASTrack",

    function(obj) {
       if(!is.null(obj@vcf.obj))
          return(length(obj@vcf.obj))
       return(NA_integer_)
       })

#----------------------------------------------------------------------------------------------------
