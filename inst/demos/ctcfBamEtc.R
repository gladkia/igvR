library(igvR)
library(MotifDb)
library(rtracklayer)
#library(AnnotationHub)
library(BSgenome.Hsapiens.UCSC.hg19)


# ---- initialDisplay
igv <- igvR()
setBrowserWindowTitle(igv, "CTCF ChIP-seq")
setGenome(igv, "hg19")
showGenomicRegion(igv, "chr3:128,079,020-128,331,275")

# ---- load the ChIP-seq readsox
roi <- getGenomicRegion(igv)
gr.roi <- with(roi, GRanges(seqnames=chrom, ranges = IRanges(start, end)))
param <- ScanBamParam(which=gr.roi, what = scanBamWhat())
bamFile <- system.file(package="igvR", "extdata", "ctcf-gata2", "gata2-region-hg19.bam")
file.exists(bamFile)
alignments <- readGAlignments(bamFile, use.names=TRUE, param=param)
track <- GenomicAlignmentTrack("ctcf bam", alignments, visibilityWindow=10000000, trackHeight=200)  # 30000 default
displayTrack(igv, track)

# --- narrow peaks obtained via macs2
narrowPeaksFile <- system.file(package="igvR", "extdata", "ctcf-gata2",
                               "gata2-region-macs2-narrowPeaks.RData")
file.exists(narrowPeaksFile)
tbl.pk <- get(load(narrowPeaksFile))
dim(tbl.pk)
track <- DataFrameQuantitativeTrack("macs2 peaks", tbl.pk, color="red", autoscale=TRUE)
displayTrack(igv, track)


# --- prepare.for.motif.match

dna <- with(roi, getSeq(BSgenome.Hsapiens.UCSC.hg19, chrom, start, end))
pfm.ctcf <- MotifDb::query(MotifDb, c("CTCF", "sapiens", "jaspar2018"), notStrings="ctcfl")
motif.name <- names(pfm.ctcf)[1]
pfm <- pfm.ctcf[[1]]

# ---- motif.match
hits.forward <- matchPWM(pfm, as.character(dna), with.score=TRUE, min.score="80%")
hits.reverse <- matchPWM(reverseComplement(pfm), as.character(dna), with.score=TRUE, min.score="80%")

tbl.forward <- as.data.frame(ranges(hits.forward))
tbl.reverse <- as.data.frame(ranges(hits.reverse))
tbl.forward$score <- mcols(hits.forward)$score
tbl.reverse$score <- mcols(hits.reverse)$score

tbl.matches <- rbind(tbl.forward, tbl.reverse)
tbl.matches$chrom <- roi$chrom
tbl.matches$start <- tbl.matches$start + roi$start

tbl.matches$end <- tbl.matches$end + roi$start

tbl.matches$name <- paste0("MotifDb::", motif.name)
tbl.matches <- tbl.matches[, c("chrom", "start", "end", "name", "score")]
dim(tbl.matches)

# ---- display scored motif hits

track <- DataFrameQuantitativeTrack("MA0139.1 scores", tbl.matches[, c(1,2,3,5)],
                                    color="random", autoscale=FALSE, min=8, max=12)
displayTrack(igv, track)

# ---- find, load, and display H3K3me3 bigwig track in this region

#  ah <- AnnotationHub()
#  ah.human <- subset(ah, species == "Homo sapiens")
#  histone.tracks <- AnnotationHub::query(ah.human, c("H3K4me3", "Gm12878", "bigwig"))

#             title
#   AH23256 | wgEncodeBroadHistoneGm12878H3k4me3StdPk.broadPeak.gz
#   AH32869 | E116-H3K4me3.fc.signal.bigwig
#   AH33901 | E116-H3K4me3.pval.signal.bigwig
#   AH40294 | E116-H3K4me3.imputed.pval.signal.bigwig
#   gr.roi <- with(getGenomicRegion(igv), GRanges(seqnames=chrom, IRanges(start, end)))
#   bw.roi <- import("/Users/paul/Library/Caches/org.R-project.R/R/AnnotationHub/154db7fc83c8f_45734.bw", which=gr.roi)
#   export.bw(bw.roi, con="gata2-region-h3k4me3.bw")

roi <- getGenomicRegion(igv)
bigwig.file <- system.file(package="igvR", "extdata", "ctcf-gata2", "gata2-region-h3k4me3.bw")
file.exists(bigwig.file)

bw.roi <- import(bigwig.file, which=gr.roi)
track <- GRangesQuantitativeTrack("h3k4me3", bw.roi, autoscale=TRUE)
displayTrack(igv, track)


## --- zoom in
showGenomicRegion(igv, "chr3:128,202,505-128,222,868")
## ----sessionInfo------------------------------------------------------------------------------------------------------
#  sessionInfo()

