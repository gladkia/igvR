igv <- start.igv("all", "hg38")
url <- "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz"
track <- GWASUrlTrack("robinson", url,chrom.col=12, pos.col=13, pval.col=28)
displayTrack(igv, track)
