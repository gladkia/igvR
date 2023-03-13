library(igvR)
igv <- igvR()
setGenome(igv, "hg38")
setBrowserWindowTitle(igv, "quantitative track demo")

tbl.bed5 <- data.frame(chr=c("1","1", "1"),
                       start=c(7432951, 7437000, 7438000),
                       end=  c(7436000, 7437500, 7440000),
                       value=c(-2.239, 3.0, 0.5),
                       sampleID=c("sample1", "sample2", "sample3"),
                       stringsAsFactors=FALSE)

showGenomicRegion(igv, "chr1:7,405,343-7,462,686")

track <- DataFrameQuantitativeTrack("bed5 +/-", tbl.bed5, color="red", trackHeight=80,
                                    autoscale=TRUE)
displayTrack(igv, track)

count <- 10000
values <- 10 * (runif(n=count) - 0.5)
#values <- c(-1, rep(1, count-1))
#values <- sin(seq_len(count))
fivenum(values)
base <- 7434000
starts <- base + seq(1, length.out=count, by=5)
ends <- starts+2
tbl.wiggish <- data.frame(chr=rep("1", count),
                          start=starts,
                          end=ends,
                          value=values,
                          name=paste0("sample-", seq_len(count)))

tbl <- tbl.bed5[, -5]
tbl <- head(tbl.wiggish)[, -5]
tbl$value <- tbl.wiggish$value[1:3]
tbl$start <- tbl.wiggish$start[1:3]
tbl$chr <- tbl.wiggish$chr[1:3]

tbl <- tbl.wiggish
track <- DataFrameQuantitativeTrack("wiggish", tbl, color="blue",
                                    trackHeight=120, autoscale=FALSE, min=-1, max=1)
displayTrack(igv, track)
showGenomicRegion(igv, "chr1:7,429,602-7,484,987")
