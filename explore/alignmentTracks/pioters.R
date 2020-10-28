library(igvR)
library(GenomicAlignments)
which <- GRanges(seqnames="chr1", ranges=IRanges(10190, 23122240))
param <- ScanBamParam(which=which, what = scanBamWhat())
bamFile <- "174.bam"
x <- readGAlignments(bamFile, use.names=TRUE, param=param)

igv <- igvR()
setGenome(igv, "hg38")
showGenomicRegion(igv, "chr1:149840-150150")
track <- GenomicAlignmentTrack("pioters", x)
displayTrack(igv, track)
