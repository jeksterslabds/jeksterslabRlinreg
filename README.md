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
    #> [1,]        1        11
    #> [2,]        1        12
    #> [3,]        1        14
    #> [4,]        1        16
    #> [5,]        1        11
    #> [6,]        1        12
    #>          wages
    #> [1,]  8.609127
    #> [2,]  2.317401
    #> [3,]  6.725796
    #> [4,] 23.144000
    #> [5,] -2.287115
    #> [6,]  5.477287

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
#> RSS            61306.05
#> MSE               47.56
#> RMSE               6.90
#> R-squared          0.22
#> Adj. R-squared     0.22
#> 
#> ANOVA Table:
#>         df       SS          MS       F            p
#> Model    1 17768.70 17768.70286 373.019 3.476513e-73
#> Error 1287 61306.05    47.63485      NA           NA
#> Total 1288 79074.75          NA      NA           NA
#> 
#> Coefficients:
#>                coef         se         t            p
#> Intercept -4.597603 0.87460976 -5.256747 1.715019e-07
#> education  1.311594 0.06791002 19.313700 3.476513e-73
#> 
#> Standardized Coefficients:
#> Textbook standard errors are used.
#>                coef         se       t            p
#> education 0.4740334 0.02454389 19.3137 3.476513e-73
#> 
#> Confidence Intervals - Regression Coefficients:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> Intercept -7.482155 -6.853794 -6.313420 -2.881785 -2.341411 -1.713050
#> education  1.087620  1.136409  1.178367  1.444820  1.486778  1.535568
#> 
#> Confidence Intervals - Standardized Slopes:
#>             ci_0.05    ci_0.5   ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> education 0.3930851 0.4107186 0.425883 0.5221838 0.5373482 0.5549817
#> 
#> Means and Standard Deviations:
#>               Mean       SD
#> wages     11.88127 7.835397
#> education 12.56400 2.831852
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
