jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-08-08

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

### Data

In this hypothetical example, we are interested in the association
between wages and education. The regressor variable is years of
education. The regressand variable is hourly wage in US dollars.

    #>      constant education
    #> [1,]        1        13
    #> [2,]        1         8
    #> [3,]        1        14
    #> [4,]        1        12
    #> [5,]        1        12
    #> [6,]        1         9
    #>          wages
    #> [1,] 14.756383
    #> [2,]  5.012765
    #> [3,] 11.764391
    #> [4,] 20.058804
    #> [5,] 10.422007
    #> [6,] -8.855241

### `jeksterslabRlinreg::linreg()`

The `jeksterslabRlinreg::linreg()` function fits a linear regression
model using `X` and `y`. In this example, `X` consists of a column of
constants and years of `education` and `y` consists of hourly `wages` in
US dollars.

The output includes the following:

  - Model assessment
  - ANOVA table
  - Table of regression coefficients with the following columns
      - Regression coefficients
      - Standard errors
      - \(t\) statistic
      - \(p\) value
      - Standardized coefficients
  - Confidence intervals (0.05, 0.5, 2.5, 97.5, 99.5, 99.95)
  - Means and standard deviations
  - Scatterplot matrix
  - Residual plots

<!-- end list -->

``` r
jeksterslabRlinreg::linreg(
  X = X,
  y = y
)
#> 
#> Model Assessment:
#>                   Value
#> RSS            60846.89
#> MSE               47.20
#> RMSE               6.87
#> R-squared          0.20
#> Adj. R-squared     0.20
#> 
#> ANOVA Table:
#>         df       SS          MS        F            p
#> Model    1 15354.35 15354.35039 324.7668 6.433283e-65
#> Error 1287 60846.89    47.27808       NA           NA
#> Total 1288 76201.24          NA       NA           NA
#> 
#> Coefficients:
#>                coef         se        t            p
#> Intercept -4.370627 0.91265598 -4.78891 1.871244e-06
#> education  1.273951 0.07069145 18.02129 6.433283e-65
#> 
#> Standardized Coefficients:
#> Textbook standard errors are used.
#>                coef         se        t            p
#> education 0.4488846 0.02490858 18.02129 6.433283e-65
#> 
#> Confidence Intervals - Regression Coefficients:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5  ci_99.5  ci_99.95
#> Intercept -7.380660 -6.724964 -6.161084 -2.580170 -2.01629 -1.360594
#> education  1.040803  1.091591  1.135268  1.412634  1.45631  1.507099
#> 
#> Confidence Intervals - Standardized Slopes:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> education 0.3667336 0.3846291 0.4000187 0.4977505 0.5131401 0.5310356
#> 
#> Means and Standard Deviations:
#>               Mean       SD
#> wages     11.71041 7.691713
#> education 12.62296 2.710223
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
