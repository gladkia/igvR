#' @name GFF3Track-class
#' @rdname GFF3Track-class
#' @exportClass GFF3Track

.GFF3Track <- setClass("GFF3Track",
                       contains="igvAnnotationTrack",
                       slots=c(
                           tbl="data.frame",
                           url="character",
                           indexURL="character",
                           colorByAttribute="character",
                           colorTable="list"
                           )
                       )
#----------------------------------------------------------------------------------------------------
#' Constructor for GFF3Track
#'
#' \code{GFF3Track} creates an \code{IGV} track for 9-column gene annotation tables
#'
#' Detailed description goes here
#'
#' @name GFF3Track
#' @rdname GFF3Track-class
#'
#' @param trackName  A character string, used as track label by igv, we recommend unique names per track.
#' @param tbl.track data.frame with 9 columns as defined at http://uswest.ensembl.org/info/website/upload/gff3.html
#' @param url character the web location of a 9-column table, gzipped or not
#' @param indexURL character the matching tabix index file
#' @param trackColor character a recognized color name or RGB triple
#' @param colorByAttribute a name from a column 9 attribute
#' @param colorTable list which maps the colorByAttribute values to different colors
#' @param displayMode "COLLAPSED", "SQUISHED" or "EXPANDED".  Spelling and case must be precise.
#' @param trackHeight track height, typically in range 20 (for annotations) and up to 1000 (for large sample vcf files)
#' @param visibilityWindow Maximum window size in base pairs for which indexed annotations or variants are displayed. Defaults: 1 MB for variants, whole chromosome for other track types.
#'
#' @return A GFF3Track object
#'
#' @examples
#' tbl.gff3 <- read.table(system.file(package="igvR", "extdata", "GRCh38.94.NDUFS2.gff3"),
#'                        sep="\t", as.is=TRUE)
#' colnames(tbl.gff3) <- c("seqid", "source", "type", "start", "end", "score", "strand",
#'                         "phase", "attributes")
#' colors <- list("antisense" = "blueviolet",
#'                "protein_coding" = "blue",
#'                "retained_intron" = "rgb(0, 150, 150)",
#'                "processed_transcript" = "purple",
#'                "processed_pseudogene" = "#7fff00",
#'                "unprocessed_pseudogene" = "#d2691e",
#'                "default" = "black")
#'  track <- GFF3Track("dataframe gff3", tbl.gff3, colorByAttribute="biotype", colorTable=colors,
#'                    url=NA_character_, indexURL=NA_character_, displayMode="EXPANDED", trackHeight=200,
#'                    visibilityWindow=100000)
#'
#' @export
#'
GFF3Track <- function(trackName, tbl.track=data.frame(), url=NA_character_, indexURL=NA_character_,
                      trackColor="black", colorByAttribute=NA_character_, colorTable=list(),
                      displayMode, trackHeight, visibilityWindow)
{
      # first figure out if the track is defined by immediate data (tbl.track not NA_character_)
      # or remote web-server provided data (url not NA_character_, indexURL optional)

  if(length(colorTable) > 0){
     sufficient.info <- TRUE
     if(is.na(colorByAttribute)){
         sufficient.info <- FALSE
     }else if (nchar(colorByAttribute) < 3){
        sufficient.info <- FALSE
        }
     if(!sufficient.info){
         msg <- sprintf("GFF3Track error: colorTable provided, by unusable 'colorByAttribute': %s",
                        colorByAttribute)
         stop(msg)
         } # !sufficient.info
     } # colorTable

   if(length(colorTable) > 0){  # igv.js expects "*" for unspecified track element types
      colorTable.default.entry <- grep("default", names(colorTable))
      if(colorTable.default.entry > 0)
          names(colorTable)[colorTable.default.entry] <- "*"
      }



   base.obj <- .igvAnnotationTrack(Track(trackType="annotation",
                                         sourceType="file",
                                         fileFormat="gff3",
                                         trackName=trackName,
                                         onScreenOrder=NA_integer_,
                                         color=trackColor,
                                         height=trackHeight,
                                         autoTrackHeight=FALSE,
                                         minTrackHeight=50,
                                         maxTrackHeight=2000,
                                         visibilityWindow=visibilityWindow),
                                   displayMode=displayMode,
                                   expandedRowHeight=30,
                                   squishedRowHeight=15,
                                   maxRows=500,
                                   searchable=FALSE
                                   )

   obj <- .GFF3Track(base.obj, tbl=tbl.track, url=url, indexURL=indexURL,
                     colorByAttribute=colorByAttribute, colorTable=colorTable)


} # GFF3Track
#----------------------------------------------------------------------------------------------------
#' Retrieve the size of the GFF3Track
#'
#' @param obj An object of class UCSCBedAnnotationTrack
#' @return The number of elements
#'
#' @examples
#' base.loc <- 88883100
#' tbl <- data.frame(chrom=rep("chr5", 3),
#'                   start=c(base.loc, base.loc+100, base.loc + 250),
#'                   end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                   name=c("a", "b", "c"),
#'                   score=runif(3),
#'                   strand=rep("*", 3),
#'                   stringsAsFactors=FALSE)
#'
#' colors <- list("antisense"="blueviolet",
#'                "protein_coding"= "blue",
#'                "retained_intron"= "rgb(0, 150, 150)",
#'                "processed_transcript"= "purple",
#'                "processed_pseudogene"= "#7fff00",
#'                "unprocessed_pseudogene"= "#d2691e",
#'                "default"= "black")
#'
#' track <- GFF3Track("dataframe gff3", tbl,
#'                    colorByAttribute="biotype", colorTable=colors,
#'                    url=NA_character_, indexURL=NA_character_,
#'                    displayMode="EXPANDED", trackHeight=200, visibilityWindow=100000)
#'
#' trackSize(track)
#'
#' @export
#'
setMethod("trackSize", "GFF3Track",

    function(obj){
       if(is.data.frame(obj@tbl)) return(nrow(obj@tbl))
       return(NA_integer_)   # we don't know the length of a remote url gff3 table
       })

#----------------------------------------------------------------------------------------------------
