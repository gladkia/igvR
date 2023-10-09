#' @name GWASTrack-class
#' @rdname GWASTrack-class
#' @exportClass GWASTrack

.GWASUrlTrack <- setClass("GWASUrlTrack",
                          contains="QuantitativeTrack",
                          slots=c(
                              coreObject="character",  # url
                              chrom.col="numeric",
                              pos.col="numeric",
                              pval.col="numeric",
                              colorTable="list")
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
#' @param url character
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
#'
#' @return A GWASUrlTrack object
#'
#' @examples
#'
#'   track <- GWASUrlTrack("GWAS from url",
#'                         "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz",
#'                          chrom.col=12, pos.col=13, pval.col=28)
#'
#'     # note: this track is autoscaled.  apparently some infinite values in the file,
#'     # leading to a flat, low track.  reproduce this in static html, report issue to igv.js
#'     # temporary workaround: use the interactive track gear to set display range.
#'
#' if(interactive()){
#'     igv <- igvR()
#'     setGenome(igv, "hg38")
#'     setBrowserWindowTitle(igv, "GWAS URL demo")
#'     displayTrack(igv, track)
#'     }
#'
#' @export
#'

GWASUrlTrack <- function(trackName,
                         url,
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

    stopifnot(is.character(url))
    stopifnot(grepl("http", url))
    obj <- .GWASUrlTrack(
              QuantitativeTrack(trackName=trackName,
                                fileFormat="gwas",
                                sourceType="url",
                                trackHeight=trackHeight,
                                #autoTrackHeight=FALSE,
                                autoscale=autoscale,
                                min=min,
                                max=max,
                                #minTrackHeight=50,
                                #maxTrackHeight=500,
                                visibilityWindow=visibilityWindow),
             coreObject=url,
             chrom.col=chrom.col,
             pos.col=pos.col,
             pval.col=pval.col,
             colorTable=colorTable)

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
