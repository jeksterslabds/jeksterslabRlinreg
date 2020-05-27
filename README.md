jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-05-27

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jeksterslabds/jeksterslabRlinreg.svg?branch=master)](https://travis-ci.com/jeksterslabds/jeksterslabRlinreg)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jeksterslabds/jeksterslabRlinreg?branch=master&svg=true)](https://ci.appveyor.com/project/jeksterslabds/jeksterslabRlinreg)
[![codecov](https://codecov.io/github/jeksterslabds/jeksterslabRlinreg/branch/master/graphs/badge.svg)](https://codecov.io/github/jeksterslabds/jeksterslabRlinreg)
<!-- badges: end -->

A collection of functions that I find useful in studying linear
regression concepts and methods.

## Installation

You can install the released version of `jeksterslabRlinreg` from
[GitHub](https://github.com/jeksterslabds/jeksterslabRlinreg) with:

``` r
library(devtools)
install_github("jeksterslabds/jeksterslabRlinreg")
```

## Example

The main function in this package is `linreg()`. It fits a linear
regression model using ordinary least squares. The required arguments
include:

  - **X**: The data matrix  is an  matrix of  observations of 
    regressors, which includes a regressor whose value is 1 for each
    observation.
  - **y**: The vector  is an  vector of observations on the regressand
    variable.

<!-- end list -->

``` r
library(jeksterslabRlinreg)
linreg(
  X = X,
  y = y,
  FUN = betahat_inv,
  output = c("coef", "model", "anova")
)
#> Coefficients:
#>   Coefficients        se         t            p
#> 1     1.314956 0.3820881  3.441501 8.518288e-04
#> 2     1.643936 0.1295125 12.693261 2.062605e-22
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.6217952
#> Adjusted coefficient of determination  0.6179360
#> Mean Squared Error                    14.2531878
#> Root Mean Squared Error                3.7753394
#> 
#> ANOVA Table:
#>  Source df       SS         MS        F            p
#>   Model  1 2343.324 2343.32410 161.1189 2.062605e-22
#>   Error 98 1425.319   14.54407       NA           NA
#>   Total 99 3768.643         NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
