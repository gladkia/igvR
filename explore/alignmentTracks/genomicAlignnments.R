library(GenomicAlignments)
library(rtracklayer)

which <- GRanges(seqnames = "chr19", ranges = IRanges(42863610, 42892559))
which <- GRanges(seqnames="chr1", ranges=IRanges(10190, 23122240))

param <- ScanBamParam(which=which)
bamFile <- "~/github/igvR/inst/extdata/LN54310_chr19.bam"
bamFile <- "174.bam"
x <- readGAlignments(bamFile, use.names=TRUE, param=param)
export(x, "psg1.bam", format="bam")


file.size("x.bam")
rawVector <- readBin("x.bam", "rb", n=file.size("x.bam"))
class(rawVector)
length(rawVector)
writeBin(rawVector, con="x2.bam")


fbin.raw <- charToRaw(fbin)

x2 <- read_file_raw("x.bam")
