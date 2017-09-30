
<!-- README.md is generated from README.Rmd. Please edit that file -->
Statistical modelling of heterogeneous citation networks
========================================================

[![Build Status](https://travis-ci.org/Selbosh/scrooge.svg?branch=master)](https://travis-ci.org/Selbosh/scrooge) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Selbosh/scrooge?branch=master&svg=true)](https://ci.appveyor.com/project/Selbosh/scrooge)

An R package containing utilities useful for the author's doctoral research in bibliometrics.

Install and load the package using

``` r
devtools::install_github('Selbosh/scrooge')
library(scrooge)
```

Bibliometric datasets
---------------------

``` r
head(articles)
#>      articles
#> AmS        49
#> AISM       52
#> AoS       101
#> ANZS       27
#> Bern       60
#> BioJ       53
```

Journal ranking metrics
-----------------------

``` r
head(names(ILSR(citations, sort = TRUE)))
#> [1] "JRSS-B" "AoS"    "Bka"    "JASA"   "Bcs"    "JRSS-A"
head(names(PageRank(citations, sort = TRUE)))
#> [1] "JASA"   "AoS"    "JRSS-B" "StMed"  "Bcs"    "Bka"
head(names(Scroogefactor(citations, sort = TRUE)))
#> [1] "JRSS-B" "AoS"    "Bka"    "JASA"   "JRSS-A" "Bcs"
```

For comparison, here are the results of analysis of the same data using the `BradleyTerry2` package and the Bradley--Terry model:

``` r
head(names(BTscores(citations)))
#> [1] "AmS"  "AISM" "AoS"  "ANZS" "Bern" "BioJ"
cor(BTscores(citations), ILSR(citations))
#> [1] 1
```
