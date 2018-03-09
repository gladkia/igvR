#----------------------------------------------------------------------------------------------------
igvBrowserFile <- system.file(package="IGV", "browserCode", "dist", "igv.html")
#----------------------------------------------------------------------------------------------------
.IGV <- setClass ("IGV",
                  representation = representation (),
                  contains = "BrowserVizClass",
                     prototype = prototype (uri="http://localhost", 9000)
                    )

#----------------------------------------------------------------------------------------------------
setGeneric('ping',         signature='obj', function (obj) standardGeneric ('ping'))
setGeneric('setGenome',    signature='obj', function (obj, genomeName) standardGeneric ('setGenome'))
setGeneric('getGenomicRegion',    signature='obj', function(obj) standardGeneric('getGenomicRegion'))
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
