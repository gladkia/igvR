library(igvR)
igv <- igvR()


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
                geneAnnotationTrackColor="darkBlue",
                initialLocus="chr5:88,621,308-89,001,037",
                visibilityWindow=5000000)

base.url <- "https://igv-data.systemsbiology.net/testFiles/sarsGenome"
fasta.file <- sprintf("%s/%s", base.url,"Sars_cov_2.ASM985889v3.dna.toplevel.fa")
fastaIndex.file <-  sprintf("%s/%s", base.url, "Sars_cov_2.ASM985889v3.dna.toplevel.fa.fai")
annotation.file <-  sprintf("%s/%s", base.url, "Sars_cov_2.ASM985889v3.101.gff3")

Sys.sleep(2)

setCustomGenome(igv,
                id="Sars_cov_2",
                genomeName="Sars_cov_2.ASM985889v3",
                fastaURL=fasta.file,
                fastaIndexURL=fastaIndex.file,
                geneAnnotationURL=annotation.file,
                geneAnnotationName="ASM985889v3",
                geneAnnotationTrackHeight=500,
                geneAnnotationTrackColor="darkBlue",
                visibilityWindow=30000)

