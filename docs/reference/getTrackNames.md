# Get the names of all the tracks currently displayed in igv

Get the names of all the tracks currently displayed in igv

## Usage

``` r
# S4 method for class 'igvR'
getTrackNames(obj)
```

## Arguments

- obj:

  An object of class igvR

## Value

A character vector

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg19")
   getTrackNames(igv)     # "Gencode v18"
   }
```
