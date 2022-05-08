igv <- start.igv("all", "hg38")
tbl.gwas <- read.table(system.file(package="igvR", "extdata", "gwas", "bellenguez.bed"),
                       sep="\t", as.is=TRUE, header=TRUE, nrow=-1)
dim(tbl.gwas)
tbl.gwasBed <- tbl.gwas[, c("chrom", "start", "end", "name", "score")]

track <- GWASTrack("bellenguuez", tbl.gwasBed,
                   chrom.col=NA_integer_, pos.col=NA_integer_, pval.col=NA_integer_)
displayTrack(igv, track)
