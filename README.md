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
    #> [1,]        1        14
    #> [2,]        1        10
    #> [3,]        1        16
    #> [4,]        1        11
    #> [5,]        1         6
    #> [6,]        1        15
    #>          wages
    #> [1,] 26.126806
    #> [2,]  0.524498
    #> [3,] 22.291331
    #> [4,] 10.053006
    #> [5,] 11.510471
    #> [6,]  9.472775

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
#> RSS            63634.05
#> MSE               49.37
#> RMSE               7.03
#> R-squared          0.22
#> Adj. R-squared     0.22
#> 
#> ANOVA Table:
#>         df       SS         MS        F            p
#> Model    1 17578.95 17578.9476 355.5346 3.223801e-70
#> Error 1287 63634.05    49.4437       NA           NA
#> Total 1288 81212.99         NA       NA           NA
#> 
#> Coefficients:
#>                coef         se         t            p
#> Intercept -3.948075 0.86444579 -4.567174 5.417175e-06
#> education  1.261570 0.06690681 18.855625 3.223801e-70
#> 
#> Standardized Coefficients:
#> Textbook standard errors are used.
#>         coef           se            t            p 
#> 4.652471e-01 2.467418e-02 1.885563e+01 3.223801e-70 
#> 
#> Confidence Intervals - Regression Coefficients:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> Intercept -6.799106 -6.178046 -5.643952 -2.252197 -1.718103 -1.097044
#> education  1.040904  1.088973  1.130311  1.392828  1.434166  1.482235
#> 
#> Confidence Intervals - Standardized Slopes:
#>   ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95 
#> 0.3838691 0.4015962 0.4168411 0.5136531 0.5288980 0.5466251 
#> 
#> Means and Standard Deviations:
#>               Mean       SD
#> wages     11.92774 7.940628
#> education 12.58417 2.928379
#> Warning in cor(x = x, y = y): the standard deviation is zero
#> Warning in cor(x, y): the standard deviation is zero
#> Warning in cor(x = x, y = y): the standard deviation is zero
#> Warning in cor(x, y): the standard deviation is zero
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
