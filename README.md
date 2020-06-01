jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-06-01

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/jeksterslabds/jeksterslabRlinreg.svg?branch=master)](https://travis-ci.com/jeksterslabds/jeksterslabRlinreg)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jeksterslabds/jeksterslabRlinreg?branch=master&svg=true)](https://ci.appveyor.com/project/jeksterslabds/jeksterslabRlinreg)
[![codecov](https://codecov.io/github/jeksterslabds/jeksterslabRlinreg/branch/master/graphs/badge.svg)](https://codecov.io/github/jeksterslabds/jeksterslabRlinreg)
<!-- badges: end -->

`jeksterslabRlinreg` is a collection of functions that I find useful in
studying linear regression concepts and methods.

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
#> 1    3.0175891 0.4069687  7.414794 5.044609e-11
#> 2    0.7122662 0.1425886  4.995252 2.667779e-06
#> 3    1.7529967 0.1334224 13.138698 4.355513e-23
#> 4    1.3611493 0.1454428  9.358654 3.882123e-15
#> 5    1.5915178 0.1390666 11.444287 1.417382e-19
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.8341273
#> Adjusted coefficient of determination  0.8271432
#> Mean Squared Error                    13.7216083
#> Root Mean Squared Error                3.7042689
#> 
#> ANOVA Table:
#>  Source df       SS        MS        F            p
#>   Model  4 6900.211 1725.0527 119.4321 3.532341e-36
#>   Error 95 1372.161   14.4438       NA           NA
#>   Total 99 8272.372        NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
