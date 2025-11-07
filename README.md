
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Norimon <img src="https://github.com/NINAnor/Norimon/blob/main/inst/figures/Norimon.png" align="right" width="160px"/>

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://img.shields.io/badge/devel%20version-1.0.1.3-blue.svg)](https://github.com/NINAnor/Norimon)
[![](https://www.r-pkg.org/badges/version/Norimon)](https://cran.r-project.org/package=Norimon)
[![R build
status](https://github.com/NINAnor/Norimon/workflows/R-CMD-check/badge.svg)](https://github.com/NINAnor/Norimon/actions)
[![](https://img.shields.io/github/languages/code-size/NINAnor/Norimon.svg)](https://github.com/NINAnor/Norimon)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16947369.svg)](https://doi.org/10.5281/zenodo.16947369)
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
#> Loading required package: rlang
connect_to_insect_db()
```

``` r
beetles <- get_observations(subset_orders = "Coleoptera")
beetles
#> # A tibble: 200 × 8
#>     year locality habitat_type region_name no_species shannon_div
#>    <int> <chr>    <chr>        <chr>            <int>       <dbl>
#>  1  2020 Skog_01  Forest       Østlandet           84        55.1
#>  2  2020 Skog_02  Forest       Østlandet          119        90.7
#>  3  2020 Skog_03  Forest       Østlandet           87        59.4
#>  4  2020 Skog_04  Forest       Østlandet           27        22.6
#>  5  2020 Skog_05  Forest       Østlandet           36        28.4
#>  6  2020 Skog_06  Forest       Østlandet           34        25.3
#>  7  2020 Skog_07  Forest       Østlandet          106        56.2
#>  8  2020 Skog_08  Forest       Østlandet           32        28.8
#>  9  2020 Skog_09  Forest       Østlandet           98        54.2
#> 10  2020 Skog_10  Forest       Østlandet           76        54.4
#> # ℹ 190 more rows
#> # ℹ 2 more variables: mean_no_asv_per_species <dbl>, GDE_by_asv <dbl>
```
