library(igvR)
igv <- igvR()
setGenome(igv, "hg38")
setBrowserWindowTitle(igv, "gwas demo")

tbl.gwas <- read.table(system.file(package="igvR", "extdata", "gwas", "bellenguez.bed"),
                       sep="\t", as.is=TRUE, header=TRUE, nrow=-1)
dim(tbl.gwas)
tbl.gwasBed <- tbl.gwas[, c("chrom", "start", "end", "name", "score")]
lapply(tbl.gwasBed, class)

track <- GWASTrack("bellenguuez", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5, trackHeight=80)
displayTrack(igv, track)

track <- GWASTrack("bellenguuez-tall", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5, trackHeight=160)
displayTrack(igv, track)

url <- "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz"
track <- GWASUrlTrack("igv sample", url,chrom.col=12, pos.col=13, pval.col=28)
displayTrack(igv, track)
