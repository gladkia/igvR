library(igvR)
igv <- igvR()
setGenome(igv, "hg38")
setBrowserWindowTitle(igv, "gwas demo")

tbl.gwas <- read.table(system.file(package="igvR", "extdata", "gwas", "bellenguez.bed"),
                       sep="\t", as.is=TRUE, header=TRUE, nrow=-1)
dim(tbl.gwas)
tbl.gwasBed <- tbl.gwas[, c("chrom", "start", "end", "name", "score")]
   # two bad rows, with chrom "0"
tbl.gwasBed <- tbl.gwasBed[grep("^chr", tbl.gwasBed$chrom),]
lapply(tbl.gwasBed, class)

track <- GWASTrack("bellenguez", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5, trackHeight=80)
displayTrack(igv, track)

    #----------------------------------------
    # alternate black and gray chrom colors
    #----------------------------------------
colors <- as.list(rep(c("black", "lightgray"), 15))
names(colors) <- paste0("chr", as.character(seq_len(length(colors))))
track <- GWASTrack("bellenguez", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5,
                   trackHeight=80, colorTable=colors)
displayTrack(igv, track)


    #----------------------------------------
    # make sure we can control trackHeight
    #----------------------------------------

track <- GWASTrack("bellenguez-tall", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5,
                   trackHeight=200)
displayTrack(igv, track)

    #----------------------------------------
    # explict y-axis scale control
    #----------------------------------------
removeTracksByName(igv, getTrackNames(igv)[3])
track <- GWASTrack("bellenguez-scale", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5,
                   trackHeight=200, autoscale=FALSE, min=0, max=100)
displayTrack(igv, track)

    #--------------------------------------------
    # explict chromosome color control,
    # color count cannot exceed chromosome count
    #--------------------------------------------

library(randomcoloR)
set.seed(17)
randomColors <- as.list(distinctColorPalette(30))  # more than we need
names(randomColors) <- as.character(seq_len(length(randomColors)))
   # 4 black chromosomes allows for a quick visual success check
randomColors[["1"]] <- "black"
randomColors[["3"]] <- "black"
randomColors[["5"]] <- "black"
randomColors[["21"]] <- "black"

chrom.randomColors <- randomColors
names(chrom.randomColors) <- paste0("chr", names(chrom.randomColors))

track <- GWASTrack("bellenguez-colors", tbl.gwasBed, chrom.col=1, pos.col=2, pval.col=5,
                   trackHeight=160, colorTable=chrom.randomColors)
displayTrack(igv, track)

removeTracksByName(igv, "bellenguuez-tall")


url <- "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz"
track <- GWASUrlTrack("igv sample", url,chrom.col=12, pos.col=13, pval.col=28,
                      trackHeight=100,
                      autoscale=FALSE, min=0, max=300,
                      colorTable=randomColors)
removeTracksByName(igv, getTrackNames(igv)[-1])

displayTrack(igv, track)


    #--------------------------------------------
    # short but continuous chromosme gwas table
    #--------------------------------------------

tbl.gwas_12 <- subset(tbl.gwas, chrom %in% c("chr1", "chr2"))

chroms <- c(rep("1", 5), rep("2", 5))
#chroms <- paste0("chr", chroms)
chroms

starts <- rep(50000001:50000010)
ends <- starts + 1
tbl.gwas_12 <- data.frame(chrom=chroms,
                          start=starts, end=ends,
                          name="fubar", score=3e-8)
with(tbl.gwas_12, end - start)
track <- GWASTrack("chr1,2", tbl.gwas_12, chrom.col=1, pos.col=2, pval.col=5,
                   trackHeight=80, autoscale=TRUE,
                   colorTable=randomColors)
removeTracksByName(igv, getTrackNames(igv)[-1])

displayTrack(igv, track)
showGenomicRegion(igv, "chr1:50000001-50000010")
showGenomicRegion(igv, "chr2:50000001-50000010")
showGenomicRegion(igv, "all")

    #--------------------------------------------------
    # now a sparse table, some chromosomes not included
    #--------------------------------------------------

chroms <- as.character(c(1:10, 13:15))
tbl.gwas.min <- data.frame(chrom=chroms, start=5000000, end=5000001, name="foo",
                           score=1.0e-10)
track <- GWASTrack("min", tbl.gwas.min, chrom.col=1, pos.col=2, pval.col=5,
                   trackHeight=80, autoscale=FALSE, min=0, max=20,
                   colorTable=randomColors)
displayTrack(igv, track)
grep("black", randomColors)  # 1, 3, 5, 21, of which the first three should be visible.

