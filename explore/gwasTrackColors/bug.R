library(igvR)
library(glue)

romanize <- function(x) {
  if (class(x) == 'integer') as.character(as.roman(x))
  else x
}

romanize2 <- function(x) x;

##### Load Yeast Genome

igv <- igvR()
setBrowserWindowTitle(igv, "Interactive Genome Browser - GWAS demo")
setGenome(igv, "sacCer3")

##### Load GWAS Tracks
PPI = 'noPPI_reference'
DRUG = 'noDrug'

MTX_infile = paste0(getwd(), glue('/data_test/{PPI}_MTX_{DRUG}_avg_logratio_Fitness_minus_ref.csv'))
MTX_table <- read.csv(MTX_infile)
tbl.yes <- MTX_table
tbl.yes$CHR <- as.character(as.roman(tbl.yes$CHR))
tbl.track <- tbl.yes[, c("CHR", "BP", "P")]
track <- GWASTrack("tbl.yes3", tbl.track, chrom.col=1, pos.col=2, pval.col=3)
displayTrack(igv, track)


MTX_table['CHR_igv'] <- as.character(unlist(lapply(MTX_table['CHR'], romanize)))
MTX_table = MTX_table[c("SNP", "CHR_igv", "BP", "EFFECTSIZE", "SE", "P", "snps_class_up", "snps_class_down", "locus_id", "GENE", "sgd_id")]

DRUG = 'noDrug'
noMTX_infile = paste0(getwd(), glue('/data_test/{PPI}_noMTX_{DRUG}_avg_logratio_Fitness_minus_ref.csv'))
noMTX_table <- read.csv(noMTX_infile)
tbl.no <- noMTX_table
tbl.no$CHR <- as.character(as.roman(tbl.no$CHR))
tbl.track <- tbl.no[, c("CHR", "BP", "P")]
track <- GWASTrack("tbl.no", tbl.track, chrom.col=1, pos.col=2, pval.col=3)
displayTrack(igv, track)



tbl.no$CHR <- as.character(as.roman(tbl.no$CHR))
tbl.track <- tbl.no[, c("CHR", "BP", "P")]
track <- GWASTrack("tbl.no", tbl.track, chrom.col=1, pos.col=2, pval.col=3)
displayTrack(igv, track)

tbl.2.chrV <- subset(tbl.track, CHR=="V")
track <- GWASTrack("tbl.no2V", tbl.2.chrV, chrom.col=1, pos.col=2, pval.col=3)
displayTrack(igv, track)
track <- GWASTrack("tbl.no3V", tbl.2.chrV, chrom.col=1, pos.col=2, pval.col=3)
displayTrack(igv, track)


write.table(tbl.2.chrV, "tbl.gwas.yeast.chrV.tsv", sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)

  # scp tbl.gwas.yeast.chrV.tsv pshannon@trena/data/dockerizedFlask/static/testFiles/gwas/
  # curl https://gladki.pl/igvR/testFiles/gwas/tbl.gwas.yeast.chrV.tsv | wc -l   # 824



MTX_track <- GWASTrack(glue('1. {PPI} under {DRUG} (MTX+)'), MTX_table, chrom.col=2, pos.col=3, pval.col=6, trackHeight=200)
displayTrack(igv, MTX_track)

DRUG = 'noDrug'
noMTX_infile = paste0(getwd(), glue('/data_test/{PPI}_noMTX_{DRUG}_avg_logratio_Fitness_minus_ref.csv'))
noMTX_table <- read.csv(noMTX_infile)
tbl.no <- noMTX_table

#noMTX_table['CHR_igv'] <- unlist(lapply(noMTX_table['CHR'], romanize))
#noMTX_table = noMTX_table[c("SNP", "CHR_igv", "BP", "EFFECTSIZE", "SE", "P", "snps_class_up", "snps_class_down", "locus_id", "GENE", "sgd_id")]
noMTX_table = noMTX_table[c("SNP", "CHR", "BP", "EFFECTSIZE", "SE", "P", "snps_class_up", "snps_class_down", "locus_id", "GENE", "sgd_id")]
noMTX_track <- GWASTrack(glue('2. {PPI} under {DRUG} (MTX+)'), noMTX_table, chrom.col=2, pos.col=3, pval.col=6, trackHeight=200)
displayTrack(igv, noMTX_track)

tbl.yes <- MTX_table
dim(tbl.yes)
head(tbl.yes)
tbl.no <- noMTX_table
dim(tbl.no)
head(tbl.no)
tbl.no$CHR <- paste0("chr", as.character(tbl.no$CHR))
no_track <- GWASTrack("tbl.no", tbl.no, chrom.col=2, pos.col=3, pval.col=6, trackHeight=200)
displayTrack(igv, no_track)
