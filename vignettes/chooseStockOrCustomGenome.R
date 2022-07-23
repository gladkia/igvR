## ----setup, include = FALSE-------------------------------------------------------------------------------------------
options(width=120)
knitr::opts_chunk$set(
   collapse = TRUE,
   eval=interactive(),
   echo=TRUE,
   comment = "#>"
)

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------
#  igv <- igvR()
#  setGenome(igv, "hg38")

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------
#  igv <- igvR()
#  setCustomGenome(igv,
#                  id="hg38",
#                  genomeName="Human (GRCh38/hg38)",
#                  fastaURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa",
#                  fastaIndexURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai",
#                  chromosomeAliasURL=NA,
#                  cytobandURL="https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg38/cytoBandIdeo.txt",
#                  geneAnnotationName="Refseq Genes",
#                  geneAnnotationURL="https://s3.amazonaws.com/igv.org.genomes/hg38/refGene.txt.gz",
#                  geneAnnotationTrackHeight=300,
#                  geneAnnotationTrackColor="darkgreen",
#                  initialLocus="chr5:88,621,308-89,001,037",
#                  visibilityWindow=5000000)

## ---- eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------
#  setCustomGenome(igv,
#                  id="hg38",
#                  genomeName="Human (GRCh38/hg38)",
#                  fastaURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa",
#                  fastaIndexURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai")

