# Constructor for RemoteAlignmentTrack

`RemoteAlignmentTrack` creates an `IGV` track for remote bam files

## Usage

``` r
RemoteAlignmentTrack(
  trackName,
  bamUrl,
  bamIndex = NULL,
  trackHeight = 50,
  visibilityWindow = 30000,
  color = "gray"
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- bamUrl:

  The URL of a bam file

- bamIndex:

  The URL of a bam index file. Defaults to \<bamUrl\>.bai

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

- color:

  A character string, either a reconized color ("red") or a hex string
  ("#FF8532")

## Value

A RemoteAlignmentTrack object

## Details

Detailed description goes here
