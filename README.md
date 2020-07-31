jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-07-31

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
    #> [1,]        1         7
    #> [2,]        1        18
    #> [3,]        1        10
    #> [4,]        1        12
    #> [5,]        1         7
    #> [6,]        1        15
    #>          wages
    #> [1,]  5.081102
    #> [2,] 12.105334
    #> [3,]  9.217860
    #> [4,]  9.086761
    #> [5,]  7.776624
    #> [6,] 24.037017

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
#> RSS            64936.35
#> MSE               50.38
#> RMSE               7.10
#> R-squared          0.19
#> Adj. R-squared     0.19
#> 
#> ANOVA Table:
#>         df       SS          MS        F            p
#> Model    1 15655.35 15655.35106 310.2798 2.185821e-62
#> Error 1287 64936.35    50.45559       NA           NA
#> Total 1288 80591.70          NA       NA           NA
#> 
#> Coefficients:
#>                coef        se         t            p
#> Intercept -3.908522 0.9081290 -4.303929 1.805086e-05
#> education  1.221423 0.0693409 17.614761 2.185821e-62
#> 
#> Standardized Coefficients:
#>         coef           se            t            p 
#> 4.407438e-01 2.502128e-02 1.761476e+01 2.185821e-62 
#> 
#> Confidence Intervals:
#>              ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5   ci_99.95
#> Intercept -6.9036249 -6.251182 -5.690098 -2.126947 -1.565863 -0.9134199
#> education  0.9927299  1.042548  1.085390  1.357457  1.400299  1.4501167
#> 
#> Confidence Intervals - Standardized Coefficients:
#>   ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95 
#> 0.3582211 0.3761976 0.3916569 0.4898308 0.5052901 0.5232666 
#> 
#> Means and Standard Deviations:
#>               Mean       SD
#> wages     11.70371 7.910196
#> education 12.78200 2.854350
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
