# see project example code:
#   https://github.com/igvteam/igv.js/blob/master/examples/events/custom-track-popover.html
#----------------------------------------------------------------------------------------------------
library(igvR)
igv <- igvR()
setGenome(igv, "hg38")
gene <- "ECRG4"
showGenomicRegion(igv, gene)
setBrowserWindowTitle(igv, "track click event demos")


file="~/github/igvR/inst/extdata/ecrg4-eqtls-popupData.RData"
tbl.eqtl <- get(load(file))
tbl.track <- tbl.eqtl[, c("chrom", "hg38", "hg38", "score", "rsid", "pvalue", "beta")]
colnames(tbl.track)[2:3] <- c("start", "end")
   # for demo purposes only:  expand variant location for easier clicking
tbl.track$start <- tbl.track$start + 5
tbl.track$end <- tbl.track$end + 5

track <- DataFrameQuantitativeTrack("eQTLs", tbl.track, autoscale=TRUE, color="black")
displayTrack(igv, track)

tbl.track2 <- tbl.track[, c("chrom", "start", "end", "rsid", "score", "pvalue", "beta")]
track2 <- DataFrameAnnotationTrack("eQTL data", tbl.track2, color="darkBlue")
displayTrack(igv, track2)

#------------------------------------------------
#  the simplest popup: display a static string
#-----------------------------------------------

js.code <- sprintf("return('%s')", "<h3>eQTL details coming soon</h3>")
x <- list(arguments="track, popoverData", body=js.code)
setTrackClickFunction(igv, x)

#------------------------------------------------
#  add a little style
#-----------------------------------------------

js.code <- sprintf("return('%s')", "<div style=\"padding: 30px; background:beige;\"><h3>eQTL details coming soon</h3></div>")
x <- list(arguments="track, popoverData", body=js.code)
setTrackClickFunction(igv, x)

#------------------------------------------------------------------------
#  traverse the data assocatiated with the feature that has been clicked
#  but just print to the javascript console for now
#-----------------------------------------------------------------------
code.lines <-  c(
        'var returnValue = "<h5> nothing yet</h5>";',
        'if(!popoverData||!popoverData.length) {',
        '  return false;',
        '}',
        'console.log(track);',
        'console.log(popoverData);',
        'var html = "<ul>";',
        'popoverData.forEach(function(nameValue){',
        '   if(nameValue.name){',
        '      html += "<li><b>" + nameValue.name + ": ";',
        '      html += nameValue.value;',
        '      html += "</li>";',
        '      }});',
        'return(html);')
js.code <- paste(code.lines, collapse="\n");
x <- list(arguments="track, popoverData", body=js.code)
setTrackClickFunction(igv, x)
