library(IGV)
library(VariantAnnotation)
library(AnnotationHub)

igv <- IGV(portRange=9000:9020)
setBrowserWindowTitle(igv, "MEF2C")
setGenome(igv, "hg19")

chrom <- "chr5"
shoulder <- 50000
start.loc <- 88013975 - shoulder
end.loc   <- 88199922 + shoulder
mef2c.region <- GRanges(seqnames=chrom, IRanges(start=start.loc, end=end.loc))

roi <- sprintf("%s:%d-%d", chrom, start.loc, end.loc)
# roi <- "chr5:87,909,738-88,281,633"
showGenomicRegion(igv, roi)

ah <- AnnotationHub()
ah.human <- subset(ah, species == "Homo sapiens")


#----------------------------------------------------------------------------------------------------
# add refseq promoters, available from RefSeq for each transcript which has been identified
#----------------------------------------------------------------------------------------------------
ah.human.refseq <- query(ah.human, "RefSeq", "hg19", "RefSeq Genes")

# download the first set

human.refseq <- ah.human.refseq[[1]]
gr.promoters <- promoters(human.refseq)
  # get rid of score, itemRgb, thick, blocks columns in the mcols, keeping just the transcript name
  # these attributes are meaningful for transcript tracks since those include the represenation
  # of UTRs, introns and exons.   but a promoter is a stretch of DNA for which those distinctions
  # do not apply
mcols(gr.promoters) <- mcols(gr.promoters)[,1]
colnames(mcols(gr.promoters)) <- "name"
ov <- findOverlaps(gr.promoters, mef2c.region)
gr.mef2c.promoters <- gr.promoters[queryHits(ov)]
track.promoters <- UCSCBedAnnotationTrack("promoters", gr.mef2c.promoters, color="darkGreen")
displayTrack(igv, track.promoters)


#----------------------------------------------------------------------------------------------------
# add H3K4me3 histone markers, two reps from the UW, another from Eik??
#----------------------------------------------------------------------------------------------------
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
                                              color=colors[color.index], trackHeight=50)
   displayTrack(igv, track.histones)
   } # for track

#----------------------------------------------------------------------------------------------------
# add DNAse Hypersensitivity regions from six sources
#----------------------------------------------------------------------------------------------------
dnase.tracks <- query(ah.human, c("Gm12878", "dnase", "narrowPeak"))
descriptions <- dnase.tracks$description
titles <- dnase.tracks$title

for(i in seq_len(length(dnase.tracks))){
   name <- names(dnase.tracks)[i]
   color.index <- color.index + 1
   gr <- dnase.tracks[[name]]
   ov <- findOverlaps(gr, mef2c.region)
   mef2c.dnase <- gr[queryHits(ov)]
   #browser()
   track.dnase <- GRangesQuantitativeTrack(titles[i], mef2c.dnase[, "signalValue"], color=colors[color.index])
   displayTrack(igv, track.dnase)
   } # for track

#----------------------------------------------------------------------------------------------------
# add IGAP gwas twice, first as an annotation track, so that names are avaialble by clicking,
# then again as a quantitative track, a -log10(pValue) score, conveying strength of association
# of that snp with the expression of the target gene, MEF2C
#----------------------------------------------------------------------------------------------------
load("~/s/work/priceLab/AD/tbl.gwas.level_1.RData")
tbl.mef2cGWAS.variants <- subset(tbl.gwas, CHR=="chr5" & oldPos > start.loc & oldPos < end.loc)
tbl.mef2cGWAS.variants.bed <- tbl.mef2cGWAS.variants[, c("CHR", "oldPos", "oldPos", "SNP", "P")]
tbl.mef2cGWAS.variants.bed$P <- -log10(tbl.mef2cGWAS.variants.bed$P)
colnames(tbl.mef2cGWAS.variants.bed) <- c("chrom", "start", "end", "name", "score")
track.gwas <- DataFrameAnnotationTrack("IGAP.gwas", tbl.mef2cGWAS.variants.bed, trackHeight=20, color="darkBlue")
displayTrack(igv, track.gwas)

tbl.mef2cGWAS.variants.bedGraph <-  tbl.mef2cGWAS.variants.bed[, -4]
track.gwas.numeric <- DataFrameQuantitativeTrack("IGAP.gwas.scored",tbl.mef2cGWAS.variants.bedGraph)
displayTrack(igv, track.gwas.numeric)

#----------------------------------------------------------------------------------------------------
# now load the relevant region of a full information vcf file, from the AMPAD project,
# 6xx? samples
#----------------------------------------------------------------------------------------------------
mef2c.gr <- GRanges(5, IRanges(start.loc, end.loc))
params <- ScanVcfParam(which=mef2c.gr)
vcfFilename <- "~/s/examples/vcf/SCH_11923_B01_GRM_WGS_2017-04-27_5.recalibrated_variants.vcf.gz"
tabixFile <- TabixFile(vcfFilename)
vcf <- readVcf(tabixFile, "hg19", params)
length(vcf)
track <- VariantTrack("AMPAD VCF", vcf, trackHeight=1000)
displayTrack(igv, track)


