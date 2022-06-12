library(igvR)
igv <- igvR()
setBrowserWindowTitle(igv, "8:12 am")
setGenome(igv, "hg38")

#showGenomicRegion(igv, "chr5:88,544,148-89,075,738")
showGenomicRegion(igv, "LACC1")

file <- system.file(package="igvR", "extdata", "tbl.mef2cGWAS.variants.RData")
tbl.gwas <- get(load(file))
dim(tbl.gwas) # 32 6
#tbl.track <- tbl.gwas[, c(1,3,3,4,9)]
#colnames(tbl.track) <- c("chrom", "start", "end", "name", "score")
colnames(tbl.gwas) <- NULL
#head(tbl.track)
track <- GWASTrack("GWAS-me", tbl.gwas, chrom.col=1, pos.col=3, pval.col=9)
displayTrack(igv, track)
head(tbl.gwas)
