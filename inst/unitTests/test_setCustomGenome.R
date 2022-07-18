library(RUnit)
library(igvR)
if(!exists("igv")){
   igv <- igvR(quiet=TRUE)
   setBrowserWindowTitle(igv, "setCustomGeneome")
   }
checkTrue(ready(igv))
allArgs <- FALSE
if(allArgs){
    setCustomGenome(igv,
                    id="hg38",
                    genomeName="Human (GRCh38/hg38)",
                    fastaURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa",
                    fastaIndexURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai",
                    cytobandURL="https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg38/cytoBandIdeo.txt",
                    chromosomeAliasURL=NA,
                    geneAnnotationName="Refseq Genes",
                    geneAnnotationURL="https://s3.amazonaws.com/igv.org.genomes/hg38/refGene.txt.gz",
                    geneAnnotationTrackHeight=500,
                    geneAnnotationTrackColor="red",
                    initialLocus="chr5:88,621,308-89,001,037",
                    visibilityWindow=5000000)

   } else {
    setCustomGenome(igv,
                    id="hg38",
                    genomeName="Human (GRCh38/hg38)",
                    fastaURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa",
                    fastaIndexURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai")
    }
# todo: follow   https://github.com/paul-shannon/igvShiny/inst/demos/customGenomes/
#   customGenome-localFiles-sars.R
#   customGenome-remoteFiles-sars.R
#
# base.url <- "https://igv-data.systemsbiology.net/testFiles/sarsGenome"
# fasta.file <- sprintf("%s/%s", base.url,"Sars_cov_2.ASM985889v3.dna.toplevel.fa")
# fastaIndex.file <-  sprintf("%s/%s", base.url, "Sars_cov_2.ASM985889v3.dna.toplevel.fa.fai")
# annotation.file <-  sprintf("%s/%s", base.url, "Sars_cov_2.ASM985889v3.101.gff3")
