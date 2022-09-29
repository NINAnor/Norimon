
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Norimon <img src="https://github.com/NINAnor/Norimon/blob/main/inst/figures/Norimon.png" align="right" width="160px"/>

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/devel%20version-0.0.0.9007-blue.svg)](https://github.com/NINAnor/Norimon)
[![](https://www.r-pkg.org/badges/version/Norimon)](https://cran.r-project.org/package=Norimon)
[![R build
status](https://github.com/NINAnor/Norimon/workflows/R-CMD-check/badge.svg)](https://github.com/NINAnor/Norimon/actions)
[![](https://img.shields.io/github/languages/code-size/NINAnor/Norimon.svg)](https://github.com/NINAnor/Norimon)
<!-- badges: end -->

Code for working with data from the Norwegian Insect Monitoring program.
This is experimental and currently only for internal use. By Jens
Åström, the Norwegian Institute for Nature Research.

## Installation

You can install the development version of Norimon from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NINAnor/Norimon")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Norimon)
#> Loading required package: ggplot2
connect_to_insect_db()
```

``` r
beetles <- get_observations(subset_orders = "Coleoptera")
beetles
#> # A tibble: 50 × 7
#>     year locality habitat_type region_name no_species shannon_div
#>    <int> <chr>    <chr>        <chr>            <int>       <dbl>
#>  1  2020 Skog_01  Forest       Østlandet           83        4.42
#>  2  2020 Skog_02  Forest       Østlandet          123        4.81
#>  3  2020 Skog_03  Forest       Østlandet           83        4.42
#>  4  2020 Skog_04  Forest       Østlandet           26        3.26
#>  5  2020 Skog_05  Forest       Østlandet           30        3.4 
#>  6  2020 Skog_06  Forest       Østlandet           33        3.5 
#>  7  2020 Skog_07  Forest       Østlandet          100        4.61
#>  8  2020 Skog_08  Forest       Østlandet           29        3.37
#>  9  2020 Skog_09  Forest       Østlandet          105        4.65
#> 10  2020 Skog_10  Forest       Østlandet           73        4.29
#> # … with 40 more rows, and 1 more variable: mean_asv_per_species <dbl>
```
