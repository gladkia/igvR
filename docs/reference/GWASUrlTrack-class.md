# Constructor for GWASUrlTrack

`GWASUrlTrack` creates an `IGV` manhattan track GWAS data

## Usage

``` r
GWASUrlTrack(
  trackName,
  url,
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

- url:

  character

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

A GWASUrlTrack object

## Examples

``` r

  track <- GWASUrlTrack("GWAS from url",
                        "https://s3.amazonaws.com/igv.org.demo/gwas_sample.tsv.gz",
                         chrom.col=12, pos.col=13, pval.col=28)

    # note: this track is autoscaled.  apparently some infinite values in the file,
    # leading to a flat, low track.  reproduce this in static html, report issue to igv.js
    # temporary workaround: use the interactive track gear to set display range.

if(interactive()){
    igv <- igvR()
    setGenome(igv, "hg38")
    setBrowserWindowTitle(igv, "GWAS URL demo")
    displayTrack(igv, track)
    }
```
