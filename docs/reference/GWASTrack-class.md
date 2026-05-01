# Constructor for GWASTrack

`GWASTrack` creates an `IGV` manhattan track GWAS data

## Usage

``` r
GWASTrack(
  trackName,
  table,
  chrom.col,
  pos.col,
  pval.col,
  colorTable = list(),
  autoscale = TRUE,
  min = 0,
  max = 10,
  trackHeight = 50,
  visibilityWindow = 1e+05
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- table:

  data.frame of 6 or more columns

- chrom.col:

  numeric, the column number of the chromosome column

- pos.col:

  numeric, the column number of the position column

- pval.col:

  numeric, the column number of the GWAS pvalue colum

- colorTable:

  a named list of CSS colors, by chromosome name - exact matches to the
  names in the GWAS table.

- autoscale:

  logical, controls how min and max of the y-axis are determined

- min:

  numeric when autoscale is FALSE, use this minimum y

- max:

  numeric when autoscale is FALSE, use this maximum y

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

A GWASTrack object

## Examples

``` r

  file <- system.file(package="igvR", "extdata", "gwas-5k.tsv")
  tbl.gwas <- read.table(file, sep="\t", header=TRUE, quote="")
  dim(tbl.gwas)
#> [1] 4949   34
  track <- GWASTrack("gwas 5k", tbl.gwas, chrom.col=12, pos.col=13, pval.col=28)

  if(interactive()){
    igv <- igvR()
    setGenome(igv, "hg38")
    setBrowserWindowTitle(igv, "GWAS demo")
    displayTrack(igv, track)
    Sys.sleep(1)  # pause before zooming in
    showGenomicRegion(igv, "chr6:32,240,829-32,929,353")
    }
```
