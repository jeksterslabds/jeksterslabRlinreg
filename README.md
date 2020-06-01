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
#> 1     3.004631 0.3438279  8.738762 7.058994e-14
#> 2     2.356432 0.1212420 19.435783 3.205082e-35
#> 3     0.898765 0.1158725  7.756498 8.801106e-12
#> 
#> Model Evaluation:
#>                                            Value
#> Coefficient of determination           0.8243828
#> Adjusted coefficient of determination  0.8207618
#> Mean Squared Error                    10.7029741
#> Root Mean Squared Error                3.2715400
#> 
#> ANOVA Table:
#>  Source df       SS         MS        F            p
#>   Model  2 5024.194 2512.09699 227.6689 2.298793e-37
#>   Error 97 1070.297   11.03399       NA           NA
#>   Total 99 6094.491         NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
