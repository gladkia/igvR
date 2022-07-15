library(AnnotationHub)

igv <- start.igv("GATA2", "hg38")
zoomOut(igv)
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

x <- aHub[["AH23388"]]

key <- "AH23388"
handle <- aHub[[key]]
   # BigWigFile object
   # resource: /Users/paul/Library/Caches/org.R-project.R/R/AnnotationHub/a99733973bce_42016

gr.hg19 <- import(handle)
system("curl -O http://hgdownload.soe.ucsc.edu/goldenPath/hg19/liftOver/hg19ToHg38.over.chain.gz")
system("gunzip hg19ToHg38.over.chain.gz")
chain <- import.chain("hg19ToHg38.over.chain")
grl.hg38 <- liftOver(gr.hg19, chain)
gr.hg38 <- unlist(grl.hg38)
seqinfo(gr.hg38) <- SeqinfoForUCSCGenome("hg38")[seqlevels(gr.hg38)]
gr.hg38.gata2.region <- gr.hg38[chrom(gr.hg38)==roi$chrom &
                                start(gr.hg38) >= roi$start &
                                end(gr.hg38)   <= roi$end]
length(gr.hg38.gata2.region)  # 4016

track <- GRangesQuantitativeTrack("h3k27ac", gr.hg38.gata2.region, autoscale=TRUE, color="green")
displayTrack(igv, track)
