library(igvR)
igv <- igvR()
setGenome(igv, "hg19")
showGenomicRegion(igv, "GATA2")

url <- "https://1000genomes.s3.amazonaws.com/phase3/data/HG02450/alignment/HG02450.mapped.ILLUMINA.bwa.ACB.low_coverage.20120522.bam"
index <- "https://1000genomes.s3.amazonaws.com/phase3/data/HG02450/alignment/HG02450.mapped.ILLUMINA.bwa.ACB.low_coverage.20120522.bam.bai"

track <- RemoteAlignmentTrack("Alignment", url, index)
displayTrack(igv, track)
