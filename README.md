jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-05-31

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
#> 1     2.008965 0.3331220  6.03072 3.112947e-08
#> 2     2.475046 0.1126943 21.96249 5.291468e-39
#> 3     2.369464 0.1177129 20.12917 4.896128e-36
#> 4     2.930873 0.1164833 25.16133 8.504350e-44
#> 5     2.700578 0.1264947 21.34934 4.977296e-38
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.9579428
#> Adjusted coefficient of determination  0.9561720
#> Mean Squared Error                    10.1480229
#> Root Mean Squared Error                3.1855962
#> 
#> ANOVA Table:
#>  Source df        SS         MS       F            p
#>   Model  4 23114.293 5778.57316 540.957 1.994773e-64
#>   Error 95  1014.802   10.68213      NA           NA
#>   Total 99 24129.095         NA      NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
