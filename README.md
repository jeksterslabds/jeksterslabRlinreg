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
#>   Coefficients         se         t            p
#> 1     2.360197 0.32399003  7.284782 9.376123e-11
#> 2     1.817457 0.10577029 17.183062 6.664414e-31
#> 3     2.336211 0.11487353 20.337247 2.211357e-36
#> 4     1.263552 0.11595872 10.896568 2.036241e-18
#> 5     1.276241 0.09952506 12.823312 1.921898e-22
#> 
#> Model Evaluation:
#>                                           Value
#> Coefficient of determination          0.9038073
#> Adjusted coefficient of determination 0.8997571
#> Mean Squared Error                    9.2445857
#> Root Mean Squared Error               3.0404910
#> 
#> ANOVA Table:
#>  Source df        SS          MS        F            p
#>   Model  4 8686.0283 2171.507085 223.1503 2.197949e-47
#>   Error 95  924.4586    9.731143       NA           NA
#>   Total 99 9610.4869          NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
