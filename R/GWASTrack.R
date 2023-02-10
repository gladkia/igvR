#' @name GWASTrack-class
#' @rdname GWASTrack-class
#' @exportClass GWASTrack

.GWASTrack <- setClass("GWASTrack",
                       contains="QuantitativeTrack",
                       slots=c(
                           coreObject="data.frame",
                           chrom.col="numeric",
                           pos.col="numeric",
                           pval.col="numeric",
                           colorTable="list")
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
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#' @param colorTable a named list of CSS colors, by chromosome name - exact matches to
#'    the names in the GWAS table.
#' @param autoscale logical, controls how min and max of the y-axis are determined
#' @param min numeric when autoscale is FALSE, use this minimum y
#' @param max numeric when autoscale is FALSE, use this maximum y
#' @return A GWASTrack object
#'
#' @examples
#'
#'   file <- system.file(package="igvR", "extdata", "gwas-5k.tsv")
#'   tbl.gwas <- read.table(file, sep="\t", header=TRUE, quote="")
#'   dim(tbl.gwas)
#'   track <- GWASTrack("gwas 5k", tbl.gwas, chrom.col=12, pos.col=13, pval.col=28)
#'
#'   if(interactive()){
#'     igv <- igvR()
#'     setGenome(igv, "hg38")
#'     setBrowserWindowTitle(igv, "GWAS demo")
#'     displayTrack(igv, track)
#'     Sys.sleep(1)  # pause before zooming in
#'     showGenomicRegion(igv, "chr6:32,240,829-32,929,353")
#'     }
#'
#' @export
#'

GWASTrack <- function(trackName,
                      table,
                      chrom.col,
                      pos.col,
                      pval.col,
                      colorTable=list(),
                      autoscale=TRUE,
                      min=0,
                      max=10,
                      trackHeight=50,
                      visibilityWindow=100000
                      )
{

    stopifnot(is.data.frame(table))
    if(length(colorTable) > 0){
        chrom.colnames <- unique(table[, chrom.col])
        color.colnames <- names(colorTable)
        unassigned.chroms <- setdiff(chrom.colnames, color.colnames)
        if(length(unassigned.chroms) > 0){
          msg <- sprintf("one or more chromsomes missing from colorTable: %s",
                         paste(unassigned.chroms, collapse=", "))
          stop(msg)
          } # if unassigned
        } # if colorTable

    obj <- .GWASTrack(QuantitativeTrack(trackName,
                                        fileFormat="gwas",
                                        sourceType="file",
                                        table,
                                        trackHeight=trackHeight,
                                        autoscale=autoscale,
                                        min=min,
                                        max=max,
                                        visibilityWindow=visibilityWindow),
                      coreObject=table,
                      chrom.col=chrom.col,
                      pos.col=pos.col,
                      pval.col=pval.col,
                      colorTable=colorTable)

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
