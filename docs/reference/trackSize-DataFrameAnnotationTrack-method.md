# Retrieve the size of the DataFrameAnnotationTrack

Retrieve the size of the DataFrameAnnotationTrack

## Usage

``` r
# S4 method for class 'DataFrameAnnotationTrack'
trackSize(obj)
```

## Arguments

- obj:

  An object of class UCSCBedAnnotationTrack

## Value

The number of elements

## Examples

``` r
base.loc <- 88883100
tbl <- data.frame(chrom=rep("chr5", 3),
                  start=c(base.loc, base.loc+100, base.loc + 250),
                  end=c(base.loc + 50, base.loc+120, base.loc+290),
                  name=c("a", "b", "c"),
                  score=runif(3),
                  strand=rep("*", 3),
                  stringsAsFactors=FALSE)

track <- DataFrameAnnotationTrack("dataframeTest", tbl)
trackSize(track)
#> [1] 3
```
