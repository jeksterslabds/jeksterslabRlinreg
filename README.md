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
#> 1     2.945563 0.3265789  9.019451 2.059867e-14
#> 2     1.339208 0.1141814 11.728773 3.581015e-20
#> 3     1.394904 0.1085540 12.849867 1.695410e-22
#> 4     2.880854 0.1075782 26.779155 4.711828e-46
#> 5     1.692121 0.1220602 13.863005 1.500754e-24
#> 
#> Model Evaluation:
#>                                           Value
#> Coefficient of determination          0.9304040
#> Adjusted coefficient of determination 0.9274737
#> Mean Squared Error                    9.4360452
#> Root Mean Squared Error               3.0718146
#> 
#> ANOVA Table:
#>  Source df         SS          MS        F            p
#>   Model  4 12614.7146 3153.678640 317.5053 4.762449e-54
#>   Error 95   943.6045    9.932679       NA           NA
#>   Total 99 13558.3191          NA       NA           NA
```

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
