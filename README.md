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
#> 1     1.421840 0.3198875  4.444811 2.382637e-05
#> 2     1.759957 0.1165189 15.104482 5.384591e-27
#> 3     1.435379 0.1169843 12.269848 2.664876e-21
#> 4     1.116707 0.1149430  9.715311 6.706031e-16
#> 5     2.789878 0.1095197 25.473764 3.058649e-44
#> 
#> Model Evaluation:
#>                                           Value
#> Coefficient of determination          0.9335460
#> Adjusted coefficient of determination 0.9307479
#> Mean Squared Error                    9.3702164
#> Root Mean Squared Error               3.0610809
#> 
#> ANOVA Table:
#>  Source df         SS          MS        F            p
#>   Model  4 13163.2728 3290.818202 333.6398 5.324549e-55
#>   Error 95   937.0216    9.863386       NA           NA
#>   Total 99 14100.2945          NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
