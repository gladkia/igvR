#' @importFrom methods new is
#' @import BiocGenerics
#' @import httpuv
#' @import BrowserViz
#' @import GenomicRanges
#' @import rtracklayer
#' @import VariantAnnotation
#' @importFrom utils write.table
#'
#' @name igvR-class
#' @rdname igvR-class
#' @exportClass igvR

.igvR <- setClass ("igvR",
                  representation = representation (),
                  contains = "BrowserVizClass",
                     prototype = prototype (uri="http://localhost", 9000)
                    )


#----------------------------------------------------------------------------------------------------
igvBrowserFile <- NULL

.onLoad <- function(...){
   igvBrowserFile <<- system.file(package="igvR", "browserCode", "dist", "igvApp.html")
   }

#----------------------------------------------------------------------------------------------------
setGeneric('ping',                 signature='obj', function (obj) standardGeneric ('ping'))
setGeneric('setGenome',            signature='obj', function (obj, genomeName) standardGeneric ('setGenome'))
setGeneric('getGenomicRegion',     signature='obj', function(obj)  standardGeneric('getGenomicRegion'))
setGeneric('showGenomicRegion',    signature='obj', function(obj, region)  standardGeneric('showGenomicRegion'))
setGeneric('displayTrack',         signature='obj', function(obj, track, deleteTracksOfSameName=TRUE) standardGeneric('displayTrack'))
setGeneric('getTrackNames',        signature='obj', function(obj) standardGeneric('getTrackNames'))
setGeneric('removeTracksByName',   signature='obj', function(obj, trackNames) standardGeneric('removeTracksByName'))
#----------------------------------------------------------------------------------------------------
setupMessageHandlers <- function()
{
   addRMessageHandler("handleResponse", "handleResponse")

} # setupMessageHandlers
#----------------------------------------------------------------------------------------------------
#' Create an igvR object
#'
#' @description
#' The igvR class provides an R interface to igv.js, a rich, interactive, full-featured, javascript
#' browser-based genome browser.  One constructs an igvR instance on a specified port (default 9000),
#' the browser code is loaded, and a websocket connection openend.  After specifying the reference
#' genome, any number of genome tracks may be created, displayed, and navigated.
#'
#' @rdname igvR-class
#'
#' @param portRange The constructor looks for a free websocket port in this range.  15000:15100 by default
#' @param host In practice, this is always "localhost"
#' @param title Used for the web browser window, "igvR" by default
#' @param browserFile The full path to the bundled html, js and libraries, and css which constitute the browser app
#' @param quiet A logical variable controlling verbosity during execution
#'
#' @return An object of the igvR class
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR(title="igv demo")
#'    Sys.sleep(2)
#'    setGenome(igv, "hg38")
#'    Sys.sleep(5)
#'    showGenomicRegion(igv, "MEF2C")
#'      #---------------------------------------------------------------
#'      # an easy transparent way to create a bed track
#'      #---------------------------------------------------------------
#'    base.loc <- 88883100
#'    tbl <- data.frame(chrom=rep("chr5", 3),
#'                      start=c(base.loc, base.loc+100, base.loc + 250),
#'                      end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                      name=c("a", "b", "c"),
#'                      score=runif(3),
#'                      strand=rep("*", 3),
#'                      stringsAsFactors=FALSE)
#'
#'    track <- DataFrameAnnotationTrack("dataframeTest", tbl, color="red", autoscale=TRUE,
#'                                      displayMode="EXPANDED")
#'    displayTrack(igv, track)
#'    showGenomicRegion(igv, sprintf("chr5:%d-%d", base.loc-100, base.loc+350))
#'    } # if !interactive
#'
#----------------------------------------------------------------------------------------------------
igvR = function(portRange=15000:15100, host="localhost", title="igvR", browserFile=igvBrowserFile,
                quiet=TRUE)
{
   if(!quiet){
      message(sprintf("want to load %s", igvBrowserFile))
      }

   obj <- .igvR(BrowserViz(portRange, title, browserFile=browserFile, quiet,
                           httpQueryProcessingFunction=myQP))
   setBrowserWindowTitle(obj, title)

   obj

} # igvR: constructor
#----------------------------------------------------------------------------------------------------
#' Test the connection between your R session and the webapp
#'
#' @rdname ping
#' @aliases ping
#'
#' @param obj An object of class igvR
#'
#' @return "pong"
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    ping(igv)
#'    }

setMethod('ping', 'igvR',

  function (obj) {
     send(obj, list(cmd="ping", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        service(100)
        }
     getBrowserResponse(obj)
     }) # ping

#----------------------------------------------------------------------------------------------------
#' Specify the reference genome, currently limited to hg38, hg19, mm10, tair10.
#'
#' @rdname setGenome
#' @aliases setGenome
#'
#' @param obj An object of class igvR
#' @param genomeName A character string, one of "hg38", "hg19", "mm10", "tair10"
#'
#' @return An empty string, an error message if the requested genome is not yet supported
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    Sys.sleep(2)
#'    setGenome(igv, "mm10")
#'    }
#'

setMethod('setGenome', 'igvR',

  function (obj, genomeName) {
     if(!obj@quiet) message(sprintf("igvR::addGenome"))
     payload <- genomeName
     send(obj, list(cmd="setGenome", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        service(100)
        }
     invisible(getBrowserResponse(obj));
     })

#----------------------------------------------------------------------------------------------------
#' Obtain the chromosome and coordiates of the currently display genomic region.
#'
#'
#' @rdname getGenomicRegion
#' @aliases getGenomicRegion
#'
#' @param obj An object of class igvR
#'
#' @return A list with four fields: chrom (character), start(numeric), end(numeric), string(character)
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    Sys.sleep(2)
#'    setGenome(igv, "hg38")
#'    Sys.sleep(5)
#'    showGenomicRegion(igv, "MEF2C")
#'    getGenomicRegion(igv)
#'      # list(chrom="chr5", start=88717241, end=88884466, string="chr5:88,717,241-88,884,466")
#'    }
#'

setMethod('getGenomicRegion', 'igvR',

   function (obj) {
     payload <- ""
     send(obj, list(cmd="getGenomicRegion", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        service(100)
        }
     x <- getBrowserResponse(obj);
     .parseChromLocString(x)
     })

#----------------------------------------------------------------------------------------------------
#' Set the visible region, by explicit chromLoc string, or by named features in any curently loaded
#' annotation tracks
#'
#' @rdname showGenomicRegion
#' @aliases showGenomicRegion
#'
#' @param obj An object of class igvR
#' @param region A genomic location (rendered "chr5:9,234,343-9,236,000" or as a list:
#' list(chrom="chr9", start=9234343, end=9236000)) or a labeled annotation in a searchable track,
#' often a gene symbol, eg "MEF2C"
#'
#' @return  ""
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    Sys.sleep(2)
#'    setGenome(igv, "hg38")
#'    Sys.sleep(5)
#'    showGenomicRegion(igv, "MEF2C")
#'    x <- getGenomicRegion(igv)
#'       #--------------------
#'       # zoom out 2kb
#'       #--------------------
#'    showGenomicRegion(igv, with(x, sprintf("%s:%d-%d", chrom, start-1000, end+1000)))
#'    }
#'
setMethod('showGenomicRegion', 'igvR',

   function (obj, region) {
      if(is.list(region)){
         valid.list <- all(c("chrom", "start", "end") %in% names(region))
         stopifnot(valid.list)
         regionString <- sprintf("%s:%d-%d", region$chrom, region$start, region$end)
         }  # if region is a list
      else if(is.character(region)) {
            regionString <- region
         }
      else{
          stop("must be a chromLoc string, e.g., 'chr1:10-60' or a search term, e.g., 'MYC'");
          }
     payload <- list(regionString=regionString)
     send(obj, list(cmd="showGenomicRegion", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        service(100)
        }
     invisible(getBrowserResponse(obj));
     })

#----------------------------------------------------------------------------------------------------
#' display the specified track in igv
#'
#' @rdname displayTrack
#' @aliases displayTrack
#'
#' @param obj An object of class igvR
#' @param track An object of some terminal (leaf) subclass of Track
#' @param deleteTracksOfSameName logical, default TRUE
#' @return  ""
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    Sys.sleep(2)
#'    setGenome(igv, "hg38")
#'    Sys.sleep(5)
#'    showGenomicRegion(igv, "MEF2C")
#'    base.loc <- 88883100
#'    tbl <- data.frame(chrom=rep("chr5", 3),
#'                      start=c(base.loc, base.loc+100, base.loc + 250),
#'                      end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                      name=c("a", "b", "c"),
#'                      score=runif(3),
#'                      strand=rep("*", 3),
#'                      stringsAsFactors=FALSE)
#'    track <- DataFrameAnnotationTrack("dataframeTest", tbl, color="red",
#'                                       autoscale=TRUE, displayMode="EXPANDED")
#'    displayTrack(igv, track)
#'    }

setMethod('displayTrack', 'igvR',

   function (obj, track, deleteTracksOfSameName=TRUE) {
     # sourceType <- track@sourceType
     # fileFormat <- track@fileFormat
     # branch and dispatch on the above 3 values

   track.info <- trackInfo(track)

   if(deleteTracksOfSameName){
      removeTracksByName(obj, track@trackName);
      }

   with(track.info,

       if(trackType == "variant" && source == "file" && fileFormat == "vcf")
          .displayVariantTrack(obj, track)

       else if(trackType == "annotation" && source == "file" && fileFormat == "bed")
          .displayAnnotationTrack(obj, track)

       else if(trackType == "quantitative" && source == "file" && fileFormat == "bedGraph")
          .displayQuantitativeTrack(obj, track)

       else if(trackType == "genomicAlignment" && source == "file" && fileFormat == "bam")
          .displayAlignmentTrack(obj, track)

       else{
          stop(sprintf("unrecogized track type, trackType: %s, source: %s, fileFormat: %s",
                       trackType, source, fileFormat))
          }
       ) # with track.info

   }) # displayTrack
#----------------------------------------------------------------------------------------------------
.displayVariantTrack <- function(igv, track)
{
   stopifnot("VariantTrack" %in% is(track))

     # we support direct and indirect variant tracks here:
     #    direct: a bioconductor VCF object is included in the track
     #  indirect: a URL and an indexURL to a vcf file on a http server is specified
     # in both cases the overall approach is the same: igv.js loads an http-served vcf
     # to make this work, a direct vcf is written to disk, below, and then served up
     # by the simple webserver built in to httpuv. since httpuv http requests do
     # not support range requests, the entire vcf file is by necessity loaded into
     # the browser.  we warn the user if they provide us with a big vcf object

   direct.unhosted.vcf <- !(is.list(track@vcf.url) && all(c("data", "index") %in% names(track@vcf.url)))

   if(direct.unhosted.vcf){
      if(length(track@vcf.obj) > 10e5)
         message(sprintf("vcf objects above %d rows may take a long time to render in igvR", 10e5))
      temp.filename <- tempfile(fileext=".vcf")
      if(!igv@quiet)
         message(sprintf("   writing vcf of size %d to %s", length(track@vcf.obj), temp.filename))
      writeVcf(track@vcf.obj, temp.filename)
      dataURL <- sprintf("%s?%s", igv@uri, temp.filename)
      indexURL <- ""
      }
   else{
      dataURL <- track@vcf.url$data
      indexURL <- track@vcf.url$index
      }

   payload <- list(name=track@trackName,
                   dataURL=dataURL,
                   indexURL=indexURL,
                   displayMode=track@displayMode,
                   color=track@color,
                   homvarColor=track@homvarColor,
                   hetvarColor=track@hetvarColor,
                   homrefColor=track@homrefColor,
                   trackHeight=200)

    send(igv, list(cmd="displayVcfTrackFromUrl", callback="handleResponse", status="request", payload=payload))

} # .displayVariantTrack
#----------------------------------------------------------------------------------------------------
.displayAlignmentTrack <- function(igv, track)
{
   stopifnot("GenomicAlignmentTrack" %in% is(track))

   if(length(track@alignment) > 10e5)
         message(sprintf("alignment objects above %d rows may take a long time to render in igvR", 10e5))
   temp.filename <- tempfile(fileext=".bam")
   if(!igv@quiet)
      message(sprintf("   writing bam file of size %d to %s", length(track@alignementj), temp.filename))
   export(track@alignment, temp.filename, format="BAM")
   dataURL <- sprintf("%s?%s", igv@uri, temp.filename)
   printf("bam url: %s", dataURL)
   indexURL <- sprintf("%s.bai", dataURL)

   payload <- list(name=track@trackName,
                   trackHeight=200,
                   dataURL=dataURL,
                   indexURL=indexURL,
                   color=track@color,
                   trackHeight=200)

    send(igv, list(cmd="displayAlignmentTrackFromUrl", callback="handleResponse", status="request", payload=payload))

} # .displayAlignmentTrack
#----------------------------------------------------------------------------------------------------
.displayAnnotationTrack <- function(igv, track)
{
   stopifnot("igvAnnotationTrack" %in% is(track))
   track.info <- trackInfo(track)

   temp.filename <- tempfile(fileext=".bed")

   if(track.info$class == "DataFrameAnnotationTrack"){
      tbl <- track@coreObject
      tbl <- tbl[order(tbl[,1], tbl[,2], decreasing=FALSE),]
      write.table(tbl, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t", file=temp.filename)
      }
   else if(track.info$class == "UCSCBedAnnotationTrack"){
      gr.bed <- track@coreObject
      export(gr.bed, temp.filename, format="BED")
      }
   else if(track.info$class == "GRangesAnnotationTrack"){
      gr.bed <- track@coreObject
      export(gr.bed, temp.filename, format="BED")
      }
   else{
      stop("cannot display annotation track of class %s", track.info$class)
      }

   if(!igv@quiet){
     message(sprintf("igvR:::.displayAnnotationTrack, temp.filename: %s", temp.filename))
     message(sprintf("       file.exists? %s", file.exists(temp.filename)))
     }

   dataURL <- sprintf("%s?%s", igv@uri, temp.filename)
   indexURL <- ""

   payload <- list(name=track@trackName,
                   dataURL=dataURL,
                   indexURL=indexURL,
                   displayMode=track@displayMode,
                   color=track@color,
                   trackHeight=track@height)

   send(igv, list(cmd="displayBedTrackFromUrl", callback="handleResponse", status="request", payload=payload))


} # .displayAnnotationTrack
#----------------------------------------------------------------------------------------------------
.displayQuantitativeTrack <- function(igv, track)
{
   stopifnot("QuantitativeTrack" %in% is(track))
   track.info <- trackInfo(track)
   stopifnot(track.info$class %in% c("DataFrameQuantitativeTrack",
                                     "UCSCBedGraphQuantitativeTrack",
                                     "GRangesQuantitativeTrack"))

   temp.filename <- tempfile(fileext=sprintf(".%s", track.info$fileFormat))

   if(track.info$class == "DataFrameQuantitativeTrack"){
      tbl <- track@coreObject
      tbl <- tbl[order(tbl[,1], tbl[,2], decreasing=FALSE),]
      write.table(tbl, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t", file=temp.filename)
      }
   else if(track.info$class == "UCSCBedGraphQuantitativeTrack"){
      gr.bedGraph <- track@coreObject
      export(gr.bedGraph, temp.filename, format="bedGraph")
      }

   else if(track.info$class == "GRangesQuantitativeTrack"){
      gr <- track@coreObject
         # identify score column.  we want just chrom, start, end, score
      if(!ncol(mcols(gr)) == 1) stop("must have exactly one numeric metadata column")
      tbl.tmp <- as.data.frame(gr)
      scores <- tbl.tmp[, ncol(tbl.tmp)]
      if(!("numeric" %in% is(scores))) stop("single metadata column, interpreted as scores, must be numeric")
      # if(diff(range(scores)) == 0) stop("bedGraph track requires variable scores in single metadata column")
      tbl.tmp <- tbl.tmp[, c(seq_len(3), ncol(tbl.tmp))]
      tbl.tmp.ordered <- tbl.tmp[order(tbl.tmp[,1], tbl.tmp[,2], decreasing=FALSE),]
      message(sprintf("writing GRangesQuantitativeTrack to %s", temp.filename))
      write.table(tbl.tmp.ordered, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, file=temp.filename)
      }

   dataURL <- sprintf("%s?%s", igv@uri, temp.filename)
   indexURL <- ""

   payload <- list(name=track@trackName,
                   fileFormat=track.info$fileFormat,
                   dataURL=dataURL,
                   indexURL=indexURL,
                   color=track@color,
                   trackHeight=track@height,
                   autoscale=track@autoscale,
                   min=track@min,
                   max=track@max)

   send(igv, list(cmd="displayQuantitativeTrackFromUrl", callback="handleResponse",
                  status="request", payload=payload))


} # .displayQuantitativeTrack
#----------------------------------------------------------------------------------------------------
#' Get the names of all the tracks currently displayed in igv
#'
#' @rdname getTrackNames
#' @aliases getTrackNames
#'
#' @param obj An object of class igvR
#'
#' @return A character vector
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    Sys.sleep(2)
#'    setGenome(igv, "hg19")
#'    Sys.sleep(5)
#'    getTrackNames(igv)     # "Gencode v18"
#'    }

setMethod('getTrackNames', 'igvR',

   function (obj) {
     payload <- ""
     send(obj, list(cmd="getTrackNames", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        service(100)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
#' Remove named tracks
#'
#' @rdname removeTracksByName
#' @aliases removeTracksByName
#'
#' @param obj An object of class igvR
#' @param trackNames a character vector
#'
#' @return A character vector
#'
#' @seealso getTrackNames
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    igv <- igvR()
#'    Sys.sleep(2)
#'    setGenome(igv, "hg19")
#'    Sys.sleep(5)   # give igv.js time to load
#     showGenomicRegion(igv, "MEF2C")
#'      # create three arbitrary tracks
#'    base.loc <- 88883100
#'    tbl <- data.frame(chrom=rep("chr5", 3),
#'                      start=c(base.loc, base.loc+100, base.loc + 250),
#'                      end=c(base.loc + 50, base.loc+120, base.loc+290),
#'                      name=c("a", "b", "c"),
#'                      score=runif(3),
#'                      strand=rep("*", 3),
#'                      stringsAsFactors=FALSE)
#'    track.1 <- DataFrameAnnotationTrack("track.1", tbl, color="red", displayMode="SQUISHED")
#'    track.2 <- DataFrameAnnotationTrack("track.2", tbl, color="blue", displayMode="SQUISHED")
#'    track.3 <- DataFrameAnnotationTrack("track.3", tbl, color="green", displayMode="SQUISHED")
#'    displayTrack(igv, track.1)
#'    displayTrack(igv, track.2)
#'    displayTrack(igv, track.3)
#'    removeTracksByName(igv, "track.2")
#'      #----------------------------------------
#'      # bulk removal of the remaining tracks,
#'      # but leave the h19 reference track
#'      #----------------------------------------
#'    removeTracksByName(igv, getTrackNames(igv)[-1])
#'    }

setMethod('removeTracksByName', 'igvR',

   function (obj, trackNames) {
     payload <- trackNames
     send(obj, list(cmd="removeTracksByName", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        service(100)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
myQP <- function(queryString)
{
   #printf("=== igvR::myQP");
   #print(queryString)
     # for reasons not quite clear, the query string comes in with extra characters
     # following the expected filename:
     #
     #  "?sampleStyle.js&_=1443650062946"
     #
     # check for that, cleanup the string, then see if the file can be found

   ampersand.loc <- as.integer(regexpr("&", queryString, fixed=TRUE))
   #printf("ampersand.loc: %d", ampersand.loc)

   if(ampersand.loc > 0){
      queryString <- substring(queryString, 1, ampersand.loc - 1);
      }

   questionMark.loc <- as.integer(regexpr("?", queryString, fixed=TRUE));
   #printf("questionMark.loc: %d", questionMark.loc)

   if(questionMark.loc == 1)
      queryString <- substring(queryString, 2, nchar(queryString))

   filename <- queryString;
   #printf("myQP filename: '%s'", filename)
   #printf("       exists?  %s", file.exists(filename));

   stopifnot(file.exists(filename))

   #printf("--- about to scan %s", filename);
      # reconstitute linefeeds though collapsing file into one string, so json
      # structure is intact, and any "//" comment tokens only affect one line
   text <- paste(scan(filename, what=character(0), sep="\n", quiet=TRUE), collapse="\n")
   #printf("%d chars read from %s", nchar(text), filename);

   return(text);

} # myQP
#----------------------------------------------------------------------------------------------------
.parseChromLocString <- function(chromLocString)
{
    chromLocString.orig <- chromLocString
    chromLocString <- gsub(",", "", chromLocString);
    tokens.0 <- strsplit(chromLocString, ":", fixed=TRUE)[[1]]
    stopifnot(length(tokens.0) == 2)
    chrom <- tokens.0[1]
    if(!grepl("chr", chrom))
        chrom <- sprintf("chr%s", chrom)

    tokens.1 <- strsplit(tokens.0[2], "-")[[1]]
    stopifnot(length(tokens.1) == 2)
    start <- as.integer(tokens.1[1])
    end <- as.integer(tokens.1[2])

    return(list(chrom=chrom, start=start, end=end, string=chromLocString.orig))

} # .parseChromLocString
#------------------------------------------------------------------------------------------------------------------------
