# Constructor for QuantitativeTrack

`QuantitativeTrack` creates an `IGV` track for genomic tracks in which a
numerical value is associated with each reported location.

## Usage

``` r
QuantitativeTrack(
  trackName,
  quantitativeData,
  fileFormat = c("wig", "bigWig", "bedGraph", "gwas"),
  color = "gray",
  sourceType = c("file", "url"),
  trackHeight = 50,
  autoscale = TRUE,
  min = NA_real_,
  max = NA_real_,
  visibilityWindow = 1e+05
)
```

## Arguments

- trackName:

  A character string, used as track label by igv, we recommend unique
  names per track.

- quantitativeData:

  A polyvalent object, either a data.frame, GRanges, or
  UCSCBedGraphQuantitative object

- fileFormat:

  only "bedGraph" supported at present; wig and bigWig support soon.

- color:

  A CSS color name (e.g., "red" or "#FF0000")

- sourceType:

  only "file" supported at present ("gcs" for Google Cloud Storage, and
  "ga4gh" for the Global Alliance API may come)

- trackHeight:

  track height, typically in range 20 (for annotations) and up to 1000
  (for large sample vcf files)

- autoscale:

  Autoscale track to maximum value in view

- min:

  Sets the minimum value for the data (y-axis) scale. Usually zero.

- max:

  Sets the maximum value for the data (y-axis) scale. This value is
  ignored if autoscale is TRUE

- visibilityWindow:

  Maximum window size in base pairs for which indexed annotations or
  variants are displayed. Defaults: 1 MB for variants, whole chromosome
  for other track types.

## Value

A QuantitativeTrack object

## Details

Detailed description will go here
