## ----setup, include = FALSE----------------------------------------------
options(width=120)
knitr::opts_chunk$set(
   collapse = TRUE,
   eval=interactive(),
   echo=TRUE,
   comment = "#>"
)


## ---- eval=TRUE, echo=FALSE----------------------------------------------
# knitr::include_graphics("igv-vignette.png")


## ----loadLibraries,  results='hide'--------------------------------------
library(igvR)
library(VariantAnnotation)
library(AnnotationHub)


## ----createLoad, results='hide'------------------------------------------
igv <- igvR()
setBrowserWindowTitle(igv, "MEF2C")
setGenome(igv, "hg19")


## ----showMEF2C,  results='hide'------------------------------------------
Sys.sleep(5)   # wait a few seconds before zooming into MEF2C
showGenomicRegion(igv, "MEF2C")


## ----roi,  results='hide'------------------------------------------------
chrom <- "chr5"
shoulder <- 50000
start.loc <- 88013975 - shoulder
end.loc   <- 88199922 + shoulder
mef2c.region <- GRanges(seqnames=chrom, IRanges(start=start.loc, end=end.loc))
showGenomicRegion(igv, list(chrom=chrom, start=start.loc, end=end.loc))


## ----data.frame.track,  results='hide'-----------------------------------
load(system.file(package="igvR", "extdata", "tbl.mef2cGWAS.variants.RData"))
tbl.mef2cGWAS.variants.bed <- tbl.mef2cGWAS.variants[, c("CHR", "oldPos", "oldPos", "SNP", "P")]
tbl.mef2cGWAS.variants.bed$P <- -log10(tbl.mef2cGWAS.variants.bed$P)
colnames(tbl.mef2cGWAS.variants.bed) <- c("chrom", "start", "end", "name", "score")
track.gwas <- DataFrameAnnotationTrack("IGAP.gwas", tbl.mef2cGWAS.variants.bed, trackHeight=20, color="darkBlue")
displayTrack(igv, track.gwas)

tbl.mef2cGWAS.variants.bedGraph <-  tbl.mef2cGWAS.variants.bed[, -4]
track.gwas.numeric <- DataFrameQuantitativeTrack("IGAP.gwas.scored", tbl.mef2cGWAS.variants.bedGraph, autoscale=TRUE)
displayTrack(igv, track.gwas.numeric)



## ----queryAHforPromoters,  results='hide'--------------------------------
ah <- AnnotationHub()
ah.human <- subset(ah, species == "Homo sapiens")
#----------------------------------------------------------------------------------------------------
# add refseq promoters, available from RefSeq for each transcript which has been identified
#----------------------------------------------------------------------------------------------------
ah.human.refseq <- query(ah.human, "RefSeq", "hg19", "RefSeq Genes")

# download the first set
human.refseq <- ah.human.refseq[[1]]
gr.promoters <- promoters(human.refseq, upstream=2000, downstream=200)
  # get rid of score, itemRgb, thick, blocks columns in the mcols, keeping just the transcript name.
  # these attributes are meaningful for transcript tracks since those include the represenation
  # of UTRs, introns and exons.   but a promoter is a stretch of DNA for which those distinctions
  # do not apply
mcols(gr.promoters) <- mcols(gr.promoters)[,1]
colnames(mcols(gr.promoters)) <- "name"
ov <- findOverlaps(gr.promoters, mef2c.region)
gr.mef2c.promoters <- gr.promoters[queryHits(ov)]
track.promoters <- UCSCBedAnnotationTrack("promoters", gr.mef2c.promoters, color="darkGreen")
displayTrack(igv, track.promoters)


## ----overlapPromotersAndVariants,  results='hide'------------------------
gr.variants <- GRanges(tbl.mef2cGWAS.variants.bed)
ov <- findOverlaps(gr.variants, gr.promoters)
gr.variantsInPromoters <- gr.variants[queryHits(ov)]
track.variantsInPromoters <-GRangesAnnotationTrack("snpInPromoters", gr.variantsInPromoters,
                                                   color="red", displayMode="EXPANDED")
displayTrack(igv, track.variantsInPromoters)


## ----methylationTracks,  results='hide'----------------------------------
histone.tracks <- query(ah.human, c("H3K4me3", "Gm12878", "Peak", "narrow"))  # 3 tracks
descriptions <- histone.tracks$description
titles <- histone.tracks$title
colors <- rep(terrain.colors(6), 4)
color.index <- 0

for(i in seq_len(length(histone.tracks))){
   name <- names(histone.tracks)[i]
   color.index <- color.index + 1
   gr <- histone.tracks[[name]]
   ov <- findOverlaps(gr, mef2c.region)
   mef2c.histones <- gr[queryHits(ov)]
   track.histones <- GRangesQuantitativeTrack(titles[i], mef2c.histones[, "pValue"],
                                              color=colors[color.index], trackHeight=50,
                                              autoscale=TRUE)
   displayTrack(igv, track.histones)
   Sys.sleep(5)
   } # for track



## ----adniVCF,  results='hide'--------------------------------------------
vcfFilename <- system.file(package="igvR", "extdata", "mef2c-4kb.vcf")
vcf <- readVcf(vcfFilename, "hg19")
track.vcf <- VariantTrack("AMPAD VCF", vcf, trackHeight=1000)
Sys.sleep(5)
displayTrack(igv, track.vcf)
Sys.sleep(10)  # allow plenty of time for this to complete, and the track to appear, before ending this chunk


## ----sessionInfo---------------------------------------------------------
sessionInfo()

