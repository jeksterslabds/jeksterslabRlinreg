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
#>   Coefficients        se        t            p
#> 1     1.133331 0.3463911 3.271826 1.476032e-03
#> 2     0.959112 0.1258016 7.624007 1.594382e-11
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.3722998
#> Adjusted coefficient of determination  0.3658947
#> Mean Squared Error                    11.7511624
#> Root Mean Squared Error                3.4279968
#> 
#> ANOVA Table:
#>  Source df        SS        MS        F            p
#>   Model  1  696.9816 696.98159 58.12548 1.594382e-11
#>   Error 98 1175.1162  11.99098       NA           NA
#>   Total 99 1872.0978        NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
