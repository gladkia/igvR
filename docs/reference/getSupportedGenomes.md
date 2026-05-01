# Get the shorthand codes (eg, "hg38") for the genomes currently supported by our use of igv.js

Get the shorthand codes (eg, "hg38") for the genomes currently supported
by our use of igv.js

## Usage

``` r
# S4 method for class 'igvR'
getSupportedGenomes(obj)
```

## Arguments

- obj:

  An object of class igvR

## Value

A character vector, the short form names of the currently supported
genomes

## Examples

``` r
if(interactive()){
   igv <- igvR()
   getSupportedGenomes(igv)
   }
```
