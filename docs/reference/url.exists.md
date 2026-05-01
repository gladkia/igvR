# url.exists

a helper function for mostly internal use, tests for availability of a
url, modeled after file.exists

a helper function for mostly internal use, tests for availability of a
url, modeled after file.exists

## Usage

``` r
url.exists(url)

url.exists(url)
```

## Arguments

- url:

  character the http address to test

## Value

logical TRUE or FALSE

logical TRUE or FALSE

## Examples

``` r

if(interactive()){
   igv <- igvR()
   ping(igv)
   }
```
