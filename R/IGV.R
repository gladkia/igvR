#----------------------------------------------------------------------------------------------------
igvBrowserFile <- system.file(package="IGV", "browserCode", "dist", "igv.html")
#----------------------------------------------------------------------------------------------------
.IGV <- setClass ("IGV",
                  representation = representation (),
                  contains = "BrowserVizClass",
                     prototype = prototype (uri="http://localhost", 9000)
                    )

#----------------------------------------------------------------------------------------------------
setGeneric('ping',                signature='obj', function (obj) standardGeneric ('ping'))
setGeneric('setGenome',           signature='obj', function (obj, genomeName) standardGeneric ('setGenome'))
setGeneric('getGenomicRegion',    signature='obj', function(obj)  standardGeneric('getGenomicRegion'))
setGeneric('showGenomicRegion',   signature='obj', function(obj, regionString)  standardGeneric('showGenomicRegion'))
setGeneric('displayTrack',        signature='obj', function(obj, track) standardGeneric('displayTrack'))
#----------------------------------------------------------------------------------------------------
setupMessageHandlers <- function()
{
   addRMessageHandler("handleResponse", "handleResponse")

} # setupMessageHandlers
#----------------------------------------------------------------------------------------------------
# constructor
IGV = function(portRange, host="localhost", title="IGV", quiet=TRUE)
{
   if(!quiet){
      printf("want to load %s", igvBrowserFile)
      }

   obj <- .IGV(BrowserViz(portRange, host, title, quiet, browserFile=igvBrowserFile,
                          httpQueryProcessingFunction=myQP))
   setBrowserWindowTitle(obj, title)

   obj

} # IGV: constructor
#----------------------------------------------------------------------------------------------------
setMethod('ping', 'IGV',

  function (obj) {
     send(obj, list(cmd="ping", callback="handleResponse", status="request", payload=""))
     while (!browserResponseReady(obj)){
        if(!obj@quiet) message(sprintf("plot waiting for browser response"));
        Sys.sleep(.1)
        }
     getBrowserResponse(obj)
     }) # ping

#----------------------------------------------------------------------------------------------------
setMethod('setGenome', 'IGV',

  function (obj, genomeName) {
     if(!obj@quiet) printf("IGV::addGenome");
     payload <- genomeName
     send(obj, list(cmd="setGenome", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('getGenomicRegion', 'IGV',

   function (obj) {
     payload <- ""
     send(obj, list(cmd="getGenomicRegion", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('showGenomicRegion', 'IGV',

   function (obj, regionString) {
     payload <- list(regionString=regionString)
     send(obj, list(cmd="showGenomicRegion", callback="handleResponse", status="request", payload=payload))
     while (!browserResponseReady(obj)){
        Sys.sleep(.1)
        }
     getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
setMethod('displayTrack', 'IGV',

   function (obj, track) {
     trackType <- track@trackType
     sourceType <- track@sourceType
     fileFormat <- track@fileFormat
       # branch and dispatch on the above 3 values

     printf("--- displayTrack:  %s + %s + %s", trackType, sourceType, fileFormat)

     if(trackType == "annotation" && sourceType == "file" && fileFormat == "bed"){
        printf ("---    display bed track")
        tbl.bed <- track@tbl
        actual.column.types <- unlist(lapply(tbl.bed[, 1:3], class), use.name=FALSE)
        good.column.types <- (actual.column.types == c("character", "numeric", "numeric")) |
                                (actual.column.types == c("character", "integer", "integer"))
        if(!(all(good.column.types)))
          stop("first 3 data.frame columns must be interpretable as chrom, start, end")
          # is the data.frame ordered?
        tbl.bed <- tbl.bed[order(tbl.bed[,2], decreasing=FALSE),]
        displayMode <- track@displayMode
        color <- track@color
        trackName <- track@trackName
        trackHeight <- track@height
        temp.filename <- sprintf("tmp%d.bed", as.integer(Sys.time()))
        if(ncol(tbl.bed) > 8)
           tbl.bed <- tbl.bed[, 1:8]
        if(!obj@quiet)
           printf("trenaViz.R about to write temporary bed file to %s", temp.filename);
         write.table(tbl.bed, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE, file=temp.filename)
         payload <- list(name=trackName, bedFileName=temp.filename, displayMode=displayMode, color=color,
                         trackHeight=trackHeight)
         send(obj, list(cmd="displayBedTrackFromFile", callback="handleResponse", status="request", payload=payload))
         } # bed format, data.frame given explicitly (not a URL)

    if(trackType == "variant" && sourceType == "file" && fileFormat == "vcf"){
       printf ("---    display vcf track")
       if(is(track@vcf.obj, "VCF")){
          if(length(track@vcf.obj) > 10e5)
             printf("vcf objects above %d rows may take a long time to render in IGV")
          temp.filename <- sprintf("tmp%d.vcf", as.integer(Sys.time()))
          trackName <- track@trackName
          printf("   writing vcf of size %d to %s", length(track@vcf.obj), temp.filename)
          writeVcf(track@vcf.obj, temp.filename)
          #payload <- list(name=trackName, vcfFileName=temp.filename)
          #send(obj, list(cmd="displayVcfTrackFromFile", callback="handleResponse", status="request", payload=payload))
          dataURL <- sprintf("http://localhost:%d?%s", obj@port, temp.filename)
          indexURL <- ""
          payload <- list(name=trackName,
                          dataURL=dataURL,
                          indexURL=indexURL,
                          displayMode="EXPANDED",
                          color="red",
                          trackHeight=200)
          send(obj, list(cmd="displayVcfTrackFromUrl", callback="handleResponse", status="request", payload=payload))
          } # track has vcf object embedded
       } # variant + file + vcf

     if(is.list(track@vcf.url) && all(c("data", "index") %in% names(track@vcf.url))){
        payload <- list(name=track@trackName,
                        dataURL=track@vcf.url$data,
                        indexURL=track@vcf.url$index,
                        displayMode=track@displayMode,
                        color=track@color,
                        trackHeight=track@height)
        send(obj, list(cmd="displayVcfTrackFromUrl", callback="handleResponse", status="request", payload=payload))
        } # vcf.url (not vcf object) supplied

     #payload
     #payload <- list(regionString=regionString)
     #send(obj, list(cmd="showGenomicRegion", callback="handleResponse", status="request", payload=payload))
     #while (!browserResponseReady(obj)){
     #   Sys.sleep(.1)
     #   }
     #getBrowserResponse(obj);
     })

#----------------------------------------------------------------------------------------------------
myQP <- function(queryString)
{
   #printf("=== IGV::myQP");
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
