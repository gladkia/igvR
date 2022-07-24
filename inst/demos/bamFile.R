library(igvR)

bamFile <- system.file(package="igvR", "extdata", "tumor.bam")
file.exists(bamFile)
igv <- igvR(quiet=FALSE)
setGenome(igv, "hg38")
little.region <- GRanges(seqnames = "21", ranges = IRanges(10399760, 10401370))
showGenomicRegion(igv, "chr21:10,397,772-10,403,883")

param <- ScanBamParam(which=little.region, what=scanBamWhat())
x <- readGAlignments(bamFile, use.names=TRUE, param=param)
track <- GenomicAlignmentTrack("bam demo", x, visibilityWindow=2000000, trackHeight=500)
displayTrack(igv, track)
