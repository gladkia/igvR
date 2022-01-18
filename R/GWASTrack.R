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
#' @param displayMode  "COLLAPSED", "SQUISHED" or "EXPANDED".  Spelling and case must be precise.
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
#'   tbl.gwas <- read.table(file, sep="\t", as.is=TRUE, header=TRUE)
#'   dim(tbl.gwas)  #  32 6
#'   track <- GWASTrack("GWAS", tbl.gwas)
#'
#'     #------------------------------------------
#'     #  show the relevant portion of the genome
#'     #------------------------------------------
#'
#'   shoulder <- 10000
#'   roi <- with(tbl.gwas, sprintf("%s:%d-%d", chrom1[1], min(start1)-shoulder, max(end2) + shoulder))
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
                      displayMode="EXPANDED",
                      visibilityWindow=100000
                                   )
{

    tradtional.gwas.format <- all(c("SNPS", "CHR_ID", "CHR_POS", "P.VALUE") %in%
                                  colnames(table))

    obj <- .GWASTrack(DataFrameAnnotationTrack(trackName, table,
                                               color=color,
                                               displayMode=displayMode,
                                               trackHeight=trackHeight,
                                               visibilityWindow=visibilityWindow),
                      chrom.col=chrom.col,
                      pos.col=pos.col,
                      pval.col=pval.col)

    obj@trackType <- "GWAS"
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
