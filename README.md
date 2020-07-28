jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-07-28

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
    #> [1,]        1        11
    #> [2,]        1         8
    #> [3,]        1         9
    #> [4,]        1        10
    #> [5,]        1        15
    #> [6,]        1        12
    #>          wages
    #> [1,]  6.045637
    #> [2,]  7.337631
    #> [3,]  4.353645
    #> [4,] -3.449000
    #> [5,] 15.772231
    #> [6,] 10.267500

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
#> RSS            62426.92
#> MSE               48.43
#> RMSE               6.96
#> R-squared          0.23
#> Adj. R-squared     0.23
#> 
#> ANOVA Table:
#>         df       SS          MS       F            p
#> Model    1 18778.38 18778.37701 387.137 1.473016e-75
#> Error 1287 62426.92    48.50577      NA           NA
#> Total 1288 81205.30          NA      NA           NA
#> 
#> Coefficients:
#>                coef        se         t            p std. coef
#> Intercept -5.532713 0.8823929 -6.270124 4.914487e-10 0.0000000
#> education  1.326519 0.0674188 19.675797 1.473016e-75 0.4808801
#> 
#> Confidence Intervals:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> Intercept -8.442935 -7.808982 -7.263799 -3.801627 -3.256444 -2.622491
#> education  1.104165  1.152601  1.194256  1.458782  1.500436  1.548873
#> 
#> Means and Standard Deviations:
#>               Mean       SD
#> wages     11.40433 7.940251
#> education 12.76804 2.878444
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
