# Set the visible region, by explicit chromLoc string, or by named features in any curently loaded annotation tracks

Set the visible region, by explicit chromLoc string, or by named
features in any curently loaded annotation tracks

## Usage

``` r
# S4 method for class 'igvR'
showGenomicRegion(obj, region)
```

## Arguments

- obj:

  An object of class igvR

- region:

  A genomic location (rendered "chr5:9,234,343-9,236,000" or as a list:
  list(chrom="chr9", start=9234343, end=9236000)) or a labeled
  annotation in a searchable track, often a gene symbol, eg "MEF2C"

## Value

""

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   showGenomicRegion(igv, "MEF2C")
   x <- getGenomicRegion(igv)
      #--------------------
      # zoom out 2kb
      #--------------------
   showGenomicRegion(igv, with(x, sprintf("%s:%d-%d", chrom, start-1000, end+1000)))
   }
```
