library(AnnotationHub)

igv <- start.igv("GATA2", "hg38")

showGenomicRegion(igv, "GATA2")
for(i in 1:4) zoomOut(igv)

roi <- getGenomicRegion(igv)
aHub <- AnnotationHub()
query.terms <- c("H3K27Ac", "k562")
length(query(aHub, query.terms))  # found 7
h3k27ac.entries <- query(aHub, query.terms)
h3k27ac.entries



   # H3K27ac is an epigenetic modification to the DNA packaging
   # protein histone H3. It is a mark that indicates acetylation of
   # the lysine residue at N-terminal position 27 of the histone H3
   # protein. H3K27ac is associated with the higher activation of
   # transcription and therefore defined as an active enhancer mark.

#             title
#  AH23388 | wgEncodeBroadHistoneK562H3k27acStdPk.broadPeak.gz
#  AH29788 | E123-H3K27ac.broadPeak.gz
#  AH30836 | E123-H3K27ac.narrowPeak.gz
#  AH31772 | E123-H3K27ac.gappedPeak.gz
#  AH32958 | E123-H3K27ac.fc.signal.bigwig
#  AH33990 | E123-H3K27ac.pval.signal.bigwig
#  AH39539 | E123-H3K27ac.imputed.pval.signal.bigwig

x.broadpeak <- aHub[["AH23388"]]
x.bigwig <- aHub[["AH32958"]]


gr.broadpeak <- x.broadpeak[seqnames(x.broadpeak)==roi$chrom &
                            start(x.broadpeak) > roi$start &
                            end(x.broadpeak) < roi$end]
   # the GRanges must have only one numeric metadata column
mcols(gr.broadPeak) <- gr.broadpeak

track <- GRangesQuantitativeTrack("h3k27ac", x.gr.sub, autoscale=TRUE, color="brown")
displayTrack(igv, track)

file.bigwig <- resource(x.bigwig)[[1]]

gr.roi <- with(roi, GRanges(seqnames=chrom, IRanges(start, end)))

gr.bw <- import(file.bigwig, which=gr.roi, format="bigwig")
track <- GRangesQuantitativeTrack("h3k27ac.bw", gr.bw, autoscale=TRUE, color="gray")
displayTrack(igv, track)
