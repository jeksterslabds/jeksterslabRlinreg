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
#> 1     3.219749 0.3580194  8.993225 2.163453e-14
#> 2     1.394658 0.1392985 10.012011 1.401451e-16
#> 3     3.091851 0.1271976 24.307466 8.340803e-43
#> 4     2.611038 0.1161088 22.487860 4.844226e-40
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.9336011
#> Adjusted coefficient of determination  0.9315261
#> Mean Squared Error                    12.0604282
#> Root Mean Squared Error                3.4728127
#> 
#> ANOVA Table:
#>  Source df        SS         MS        F            p
#>   Model  3 16957.546 5652.51548 449.9355 2.216008e-56
#>   Error 96  1206.043   12.56295       NA           NA
#>   Total 99 18163.589         NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
