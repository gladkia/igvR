## ----setup, include = FALSE----------------------------------------------
options(width=120)
knitr::opts_chunk$set(
   collapse = TRUE,
   eval=interactive(),
   echo=TRUE,
   comment = "#>"
)


## ---- eval=TRUE, echo=FALSE----------------------------------------------
knitr::include_graphics("ctcf-chip-seq-gata2-252kb-igvR.png")


## ----loadLibraries,  results='hide'--------------------------------------
library(igvR)
library(MotifDb)
library(BSgenome.Hsapiens.UCSC.hg19)


## ----createLoad, results='hide'------------------------------------------
igv <- igvR()
setBrowserWindowTitle(igv, "CTCF ChIP-seq")
setGenome(igv, "hg19")


## ----initialDisplay,  results='hide'-------------------------------------
showGenomicRegion(igv, "chr3:128,079,020-128,331,275")


## ----roi,  results='hide'------------------------------------------------
loc <- getGenomicRegion(igv)
which <- with(loc, GRanges(seqnames=chrom, ranges = IRanges(start, end)))
param <- ScanBamParam(which=which, what = scanBamWhat())
bamFile <- "~/github/ChIPseqMotifMatch/bulk/GSM749704/GSM749704_hg19_wgEncodeUwTfbsGm12878CtcfStdAlnRep1.bam"
file.exists(bamFile)
x <- readGAlignments(bamFile, use.names=TRUE, param=param)
track <- GenomicAlignmentTrack("ctcf bam", x, visibilityWindow=10000000, trackHeight=200)  # 30000 default
displayTrack(igv, track)


## ----narrow.peaks.track,  results='hide'---------------------------------
filename <- system.file(package="igvR", "extdata", "GSM749704_hg19_chr19_peaks.narrowPeak")
tbl.pk <- read.table(filename, sep="\t", header=FALSE, as.is=TRUE)[, c(1,2,3,5)]
dim(tbl.pk)
colnames(tbl.pk) <- c("chrom", "start", "end", "score")
loc <- getGenomicRegion(igv)
while(!is.list(loc)){
   print(loc)
   loc <- getGenomicRegion(igv)
   }
printf("--- good loc obtained? ")
print(loc)

tbl.pk <- subset(tbl.pk, chrom==loc$chrom & start >= loc$start & end <= loc$end)
dim(tbl.pk)
track <- DataFrameQuantitativeTrack("macs2 peaks", tbl.pk, color="red", autoscale=TRUE)
displayTrack(igv, track)


## ----motif.match.and.display, results='hide'-----------------------------

dna <- with(loc, getSeq(BSgenome.Hsapiens.UCSC.hg19, chrom, start, end))


pfm.ctcf <- query(MotifDb, c("CTCF", "sapiens", "jaspar2018"), notStrings="ctcfl")
motif.name <- names(pfm.ctcf)[1]
pfm <- pfm.ctcf[[1]]

hits.forward <- matchPWM(pfm, as.character(dna), with.score=TRUE, min.score="80%")
hits.reverse <- matchPWM(reverseComplement(pfm), as.character(dna), with.score=TRUE, min.score="80%")

tbl.forward <- as.data.frame(ranges(hits.forward))
tbl.reverse <- as.data.frame(ranges(hits.reverse))
tbl.forward$score <- mcols(hits.forward)$score
tbl.reverse$score <- mcols(hits.reverse)$score

tbl.matches <- rbind(tbl.forward, tbl.reverse)


tbl.matches$chrom <- loc$chrom
tbl.matches$start <- tbl.matches$start + loc$start
tbl.matches$end <- tbl.matches$end + loc$start
with(tbl.matches, end-start)

enableMotifLogoPopups(igv)

tbl.matches$name <- paste0("MotifDb::", motif.name)
tbl.matches <- tbl.matches[, c("chrom", "start", "end", "name", "score")]
dim(tbl.matches)

head(tbl.matches)
track <- DataFrameAnnotationTrack("CTCF-MA0139.1", tbl.matches, color="random")
displayTrack(igv, track)

track <- DataFrameQuantitativeTrack("MA0139.1 scores", tbl.matches[, c(1,2,3,5)], color="random", autoscale=FALSE, min=8, max=12)
displayTrack(igv, track)



## ----sessionInfo---------------------------------------------------------
sessionInfo()

