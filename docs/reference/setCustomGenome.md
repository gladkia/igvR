# Specify the reference genome you wish to use, via full specification of all urls

Specify the reference genome you wish to use, via full specification of
all urls

## Usage

``` r
# S4 method for class 'igvR'
setCustomGenome(
  obj,
  id,
  genomeName,
  fastaURL,
  fastaIndexURL,
  chromosomeAliasURL = NA,
  cytobandURL = NA,
  geneAnnotationName = NA,
  geneAnnotationURL = NA,
  geneAnnotationTrackHeight = 200,
  geneAnnotationTrackColor = "darkblue",
  initialLocus = "all",
  visibilityWindow = 1e+06
)
```

## Arguments

- obj:

  An object of class igvR

- id:

  character string, a short name, displayed in the browser, e.g.,
  "hg38", "tair10".

- genomeName:

  character string, possibly longer, more descirptive then the id, e.g.,
  "Human (GRCh38/hg38)"

- fastaURL:

  character string,
  e.g."https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa"

- fastaIndexURL:

  character string, e.g.
  "https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai"

- chromosomeAliasURL:

  character string, default NA, a tab-delimited file supporting multiple
  equivalent chromosome names. see details

- cytobandURL:

  character string, default NA, a cytoband ideogram file in UCSC format,
  e.g.
  "https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg38/cytoBandIdeo.txt"

- geneAnnotationName:

  character string, e.g. "Refseq Genes", default NA

- geneAnnotationURL:

  character string, e.g.
  "https://s3.amazonaws.com/igv.org.genomes/hg38/refGene.txt.gz",
  default NA

- geneAnnotationTrackHeight:

  numeric, pixels, e.g. 500. default 200

- geneAnnotationTrackColor:

  character string, any legal CSS color, default "darkblue"

- initialLocus:

  character string, e.g. "chr5:88,621,308-89,001,037" or "MEF2C"

- visibilityWindow:

  numeric, number of bases over which to display features, default
  1000000

## Value

An empty string, an error message if any of the urls could not be
reached

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setCustomGenome(igv,
                   id="hg38",
                   genomeName="Human (GRCh38/hg38)",
                   fastaURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa",
                   fastaIndexURL="https://s3.amazonaws.com/igv.broadinstitute.org/genomes/seq/hg38/hg38.fa.fai",
                   chromosomeAliasURL=NA,
                   cytobandURL="https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg38/cytoBandIdeo.txt",
                   geneAnnotationName="Refseq Genes",
                   geneAnnotationURL="https://s3.amazonaws.com/igv.org.genomes/hg38/refGene.txt.gz",
                   geneAnnotationTrackHeight=300,
                   geneAnnotationTrackColor="darkgreen",
                   initialLocus="chr5:88,621,308-89,001,037",
                   visibilityWindow=5000000)
   }
```
