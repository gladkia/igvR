# Constructor for GFF3Track

`GFF3Track` creates an `IGV` track for 9-column gene annotation tables

## Usage

``` r
GFF3Track(
  trackName,
  tbl.track = data.frame(),
  url = NA_character_,
  indexURL = NA_character_,
  trackColor = "black",
  colorByAttribute = NA_character_,
  colorTable = list(),
  displayMode,
  trackHeight,
  visibilityWindow
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- tbl.track:

  data.frame with 9 columns as defined at
  http://uswest.ensembl.org/info/website/upload/gff3.html

- url:

  character the web location of a 9-column table, gzipped or not

- indexURL:

  character the matching tabix index file

- trackColor:

  character a recognized color name or RGB triple

- colorByAttribute:

  a name from a column 9 attribute

- colorTable:

  list which maps the colorByAttribute values to different colors

- displayMode:

  "COLLAPSED", "SQUISHED" or "EXPANDED". Spelling and case must be
  precise.

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

A GFF3Track object

## Details

Detailed description goes here

## Examples

``` r
tbl.gff3 <- read.table(system.file(package="igvR", "extdata", "GRCh38.94.NDUFS2.gff3"),
                       sep="\t", as.is=TRUE)
colnames(tbl.gff3) <- c("seqid", "source", "type", "start", "end", "score", "strand",
                        "phase", "attributes")
colors <- list("antisense" = "blueviolet",
               "protein_coding" = "blue",
               "retained_intron" = "rgb(0, 150, 150)",
               "processed_transcript" = "purple",
               "processed_pseudogene" = "#7fff00",
               "unprocessed_pseudogene" = "#d2691e",
               "default" = "black")
track <- GFF3Track("dataframe gff3", tbl.gff3, colorByAttribute="biotype", colorTable=colors,
                   url=NA_character_, indexURL=NA_character_, displayMode="EXPANDED", trackHeight=200,
                   visibilityWindow=100000)

   # gff3 table structure is not bed-like. find chrom, start, end as seen below

roi <- with(tbl.gff3, sprintf("%s:%d-%d",
                              seqid[1],
                              as.integer(min(start)) - 1000,
                              as.integer(max(end)) + 1000))
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   setBrowserWindowTitle(igv, "GWAS demo")
   showGenomicRegion(igv, roi)
   displayTrack(igv, track)
   }
```
