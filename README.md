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
#>   Coefficients       se         t            p
#> 1     2.368265 0.356018  6.652092 1.660003e-09
#> 2     2.849584 0.116708 24.416343 1.888785e-43
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.8588217
#> Adjusted coefficient of determination  0.8573811
#> Mean Squared Error                    12.4156790
#> Root Mean Squared Error                3.5235889
#> 
#> ANOVA Table:
#>  Source df       SS         MS        F            p
#>   Model  1 7552.759 7552.75891 596.1578 1.888785e-43
#>   Error 98 1241.568   12.66906       NA           NA
#>   Total 99 8794.327         NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
