# Constructor for VariantTrack

`VariantTrack` creates an `IGV` track for VCF (variant call format)
objects, either local or at a remote url

## Usage

``` r
VariantTrack(
  trackName,
  vcf,
  trackHeight = 50,
  anchorColor = "pink",
  homvarColor = "rgb(17,248,254)",
  hetvarColor = "rgb(34,12,253)",
  homrefColor = "rgb(200,200,200)",
  displayMode = "EXPANDED",
  visibilityWindow = 1e+05
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- vcf:

  A VCF object from the VariantAnnotation package, or a list(url=x,
  index=y) pointing to a vcf file

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- anchorColor:

  CSS color name (e.g., "red" or "#FF0000") for the "anchoring"
  graphical segment in the track

- homvarColor:

  CSS color name for homozygous variant samples, rgb(17,248,254) by
  default (~turquoise)

- hetvarColor:

  CSS color name for heterzygous variant samples, rgb(34,12,253) by
  default (~royalBlue)

- homrefColor:

  CSS color names for homozygous reference samples, rgb(200,200,200) by
  default (~lightGray)

- displayMode:

  "COLLAPSED", "EXPANDED", or "SQUISHED"

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

A VariantTrack object

## Details

Detailed description goes here

## Examples

``` r

    #----------------------------
    #  first, from a local file
    #----------------------------

f <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
roi <- GRanges(seqnames="22", ranges=IRanges(start=c(50301422, 50989541),
                                              end=c(50312106, 51001328),
                                              names=c("gene_79087", "gene_644186")))
vcf.sub <- VariantAnnotation::readVcf(f, "hg19", param=roi)
track.local <- VariantTrack("chr22-tiny", vcf.sub)

    #----------------------------
    # now try a url track
    #----------------------------

data.url <- sprintf("%s/%s", "https://s3.amazonaws.com/1000genomes/release/20130502",
                               "ALL.wgs.phase3_shapeit2_mvncall_integrated_v5b.20130502.sites.vcf.gz")
index.url <- sprintf("%s.tbi", data.url)
url <- list(data=data.url, index=index.url)

track.url <- VariantTrack("1kg", url)
```
