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
#> 1     1.804000 0.3713899  4.857430 4.562732e-06
#> 2     2.126477 0.1281619 16.592124 4.381701e-30
#> 3     1.160251 0.1178413  9.845884 2.884924e-16
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.7897203
#> Adjusted coefficient of determination  0.7853846
#> Mean Squared Error                    12.7947166
#> Root Mean Squared Error                3.5769703
#> 
#> ANOVA Table:
#>  Source df       SS         MS        F            p
#>   Model  2 4805.145 2402.57266 182.1451 1.431117e-33
#>   Error 97 1279.472   13.19043       NA           NA
#>   Total 99 6084.617         NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
