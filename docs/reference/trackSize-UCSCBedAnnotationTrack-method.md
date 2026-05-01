# Retrieve the size of theUCSCBedAnnotationTrack

Retrieve the size of theUCSCBedAnnotationTrack

## Usage

``` r
# S4 method for class 'UCSCBedAnnotationTrack'
trackSize(obj)
```

## Arguments

- obj:

  An object of class UCSCBedAnnotationTrack

## Value

The number of elements

## Examples

``` r
bed.filepath <- system.file(package = "rtracklayer", "tests", "test.bed")
gr.bed <- rtracklayer::import(bed.filepath)
track.1 <- UCSCBedAnnotationTrack("UCSC bed", gr.bed,  color="blue", displayMode="SQUISHED")
trackSize(track.1)
#> [1] 5
```
