# Obtain the chromosome and coordiates of the currently displayed genomic region.

Some caution is needed with this function when called right after a
lengthy browser operation - of which the main example is display a
GenomicAlignmentTrack. igv.js does not at present allow us to delay the
return from javascript pending completion of the track rendering. This
does not pose much of a problem when you manipulate igv in the browser
from R in normal interactive mode: simply wait for your last command to
complete. But if you are running in programmatic mode, as we do when
testing igvR, then caution is advised. See the test_displayAlignment
function in unitTests/test_igvR.R.

## Usage

``` r
# S4 method for class 'igvR'
getGenomicRegion(obj)
```

## Arguments

- obj:

  An object of class igvR

## Value

A list with four fields: chrom (character), start(numeric),
end(numeric), string(character)

## Examples

``` r
if(interactive()){
   igv <- igvR()
   setGenome(igv, "hg38")
   showGenomicRegion(igv, "MEF2C")
   getGenomicRegion(igv)
     # list(chrom="chr5", start=88717241, end=88884466, string="chr5:88,717,241-88,884,466")
   }
```
