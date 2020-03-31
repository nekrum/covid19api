# covid19api

<!-- badges: start -->
<!-- badges: end -->

Use https://covid19api.com/ API which provide data from [Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19)

This R api package was created was created using the `httr()` vignette 
[Best practices for API packages](https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html)

## Installation

You can install covid19api package with:

``` r
remotes::install_github("nekrum/covid19api")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(covid19api)
CovidAPI("summary")
```

> [CovidImage from wikimedia](https://commons.wikimedia.org/wiki/File:Coronavirus_SARS-CoV-2.jpg)
