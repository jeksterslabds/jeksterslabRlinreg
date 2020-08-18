jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-08-19

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
    #> [1,]        1        12
    #> [2,]        1        12
    #> [3,]        1        12
    #> [4,]        1        15
    #> [5,]        1        17
    #> [6,]        1        15
    #>          wages
    #> [1,] 16.252610
    #> [2,]  5.175189
    #> [3,]  9.017987
    #> [4,] 20.491590
    #> [5,] 25.288857
    #> [6,] 13.725182

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
#> RSS            62721.58
#> MSE               48.66
#> RMSE               6.98
#> R-squared          0.22
#> Adj. R-squared     0.22
#> 
#> ANOVA Table:
#>         df       SS          MS        F            p
#> Model    1 17473.48 17473.47815 358.5427 9.899242e-71
#> Error 1287 62721.58    48.73472       NA           NA
#> Total 1288 80195.06          NA       NA           NA
#> 
#> Coefficients:
#>                coef         se         t            p
#> Intercept -4.956494 0.90469820 -5.478616 5.151095e-08
#> education  1.317453 0.06957684 18.935224 9.899242e-71
#> 
#> Standardized Coefficients:
#> Textbook standard errors are used.
#>                coef         se        t            p
#> education 0.4667839 0.02465162 18.93522 9.899242e-71
#> 
#> Confidence Intervals - Regression Coefficients:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> Intercept -7.940281 -7.290303 -6.731339 -3.181649 -2.622685 -1.972707
#> education  1.087981  1.137969  1.180957  1.453950  1.496937  1.546925
#> 
#> Confidence Intervals - Standardized Slopes:
#>             ci_0.05    ci_0.5    ci_2.5   ci_97.5   ci_99.5  ci_99.95
#> education 0.3854803 0.4031912 0.4184221 0.5151457 0.5303766 0.5480875
#> 
#> Means and Standard Deviations:
#>               Mean       SD
#> wages     11.77383 7.890706
#> education 12.69899 2.795739
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
