library(igvR)
igv <- igvR()
setGenome(igv, "hg38")
showGenomicRegion(igv, "GATA2")

tbl <- data.frame(chr=rep("chr2", 3),
                  start=c(16102928, 16101906, 16102475),
                  end=  c(16102941, 16101917, 16102484),
                  value=c(2, 5, 19),
                  stringsAsFactors=FALSE)

showGenomicRegion(igv, sprintf("chr2:%d-%d", min(tbl$start)-50, max(tbl$end)+50))
track <- DataFrameQuantitativeTrack("autoScale", tbl, autoscale=TRUE)
displayTrack(igv, track)


bamFile <- system.file(package="igvR", "extdata", "tumor.bam")
stopifnot(file.exists(bamFile))
which <- GRanges(seqnames = "21", ranges = IRanges(10400126, 10400326))
showGenomicRegion(igv, "chr21:10,399,824-10,400,627")

param <- ScanBamParam(which=which, what = scanBamWhat())
x <- readGAlignments(bamFile, use.names=TRUE, param=param)
track <- GenomicAlignmentTrack("tumor", x, trackHeight=200)
displayTrack(igv, track)
