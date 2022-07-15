igv <- start.igv("all", "hg38")
tbl.gwas <- read.table(system.file(package="igvR", "extdata", "gwas", "bellenguez.bed"),
                       sep="\t", as.is=TRUE, header=TRUE, nrow=-1)
dim(tbl.gwas)
tbl.gwasBed <- tbl.gwas[, c("chrom", "start", "end", "name", "score")]

track <- GWASTrack("bellenguuez", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5, trackHeight=80)
displayTrack(igv, track)

track <- GWASTrack("bellenguuez-tall", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5, trackHeight=160)
displayTrack(igv, track)
