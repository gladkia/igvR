# browse to https://genome.ucsc.edu/cgi-bin/hgTables
# choose track and table, paste in region from igv
#  set output format: "data points" to get the measured values in column 4
# click on thhe "get output" button,  download to, e.g.,  ~/drop/k562-h3k4me3-gata2.tsv

library(igvR)
igv <- igvR()
setBrowserWindowTitle(igv, "H3K4Me3 GATA2")
setGenome(igv, "hg38")
showGenomicRegion(igv, "GATA2")
zoomOut(igv)
zoomOut(igv)
getGenomicRegion(igv)


f <- system.file(package="igvR", "extdata", "k562-h3k4me3-gata2.tsv")
tbl <- read.table(f, sep="\t", skip=1, as.is=TRUE, fill=TRUE)
colnames(tbl) <- c("chrom", "start", "end", "score")

head(tbl)
lapply(tbl, class)

track <- DataFrameQuantitativeTrack("H3K4Me3 K562", tbl, autoscale=TRUE, color="darkGreen")
displayTrack(igv, track)

