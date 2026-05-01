# Specify the reference genome, currently limited to hg38, hg19, mm10, tair10.

Specify the reference genome, currently limited to hg38, hg19, mm10,
tair10.

## Usage

``` r
# S4 method for class 'igvR'
setGenome(obj, genomeName)
```

## Arguments

- obj:

  An object of class igvR

- genomeName:

  A character string, one of "hg38", "hg19", "mm10", "tair10"

## Value

An empty string, an error message if the requested genome is not yet
supported

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "mm10")
   }
```
