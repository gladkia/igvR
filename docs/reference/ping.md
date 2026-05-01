# Test the connection between your R session and the webapp

Test the connection between your R session and the webapp

## Usage

``` r
# S4 method for class 'igvR'
ping(obj, msecDelay = 0)
```

## Arguments

- obj:

  An object of class igvR

- msecDelay:

  don't return until these many milliseconds have passed, default 0

## Value

"pong"

## Examples

``` r
if(interactive()){
   igv <- igvR()
   ping(igv)
   }
```
