jeksterslabRlinreg
================
Ivan Jacob Agaloos Pesigan
2020-08-29

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

Installation
------------

You can install the released version of `jeksterslabRlinreg` from
[GitHub](https://github.com/jeksterslabds/jeksterslabRlinreg) with:

    library(devtools)
    install_github("jeksterslabds/jeksterslabRlinreg")

Example
-------

### Data

In this hypothetical example, we are interested in the association
between wages and education. The regressor variable is years of
education. The regressand variable is hourly wage in US dollars.

    #>      constant education
    #> [1,]        1         7
    #> [2,]        1        14
    #> [3,]        1        15
    #> [4,]        1        13
    #> [5,]        1        15
    #> [6,]        1         8
    #>           wages
    #> [1,]  0.5469977
    #> [2,] 20.4037125
    #> [3,] 30.6881340
    #> [4,] 13.6174883
    #> [5,] 18.5998624
    #> [6,]  6.5390001

### `jeksterslabRlinreg::linreg()`

The `jeksterslabRlinreg::linreg()` function fits a linear regression
model using `X` and `y`. In this example, `X` consists of a column of
constants and years of `education` and `y` consists of hourly `wages` in
US dollars.

The output includes the following:

-   Model assessment
-   ANOVA table
-   Table of regression coefficients with the following columns
    -   Regression coefficients
    -   Standard errors
    -   *t* statistic
    -   *p* value
    -   Standardized coefficients
-   Confidence intervals (0.05, 0.5, 2.5, 97.5, 99.5, 99.95)
-   Means and standard deviations
-   Scatterplot matrix
-   Residual plots

<!-- -->

    jeksterslabRlinreg::linreg(
      X = X,
      y = y
    )

See [GitHub
Pages](https://jeksterslabds.github.io/jeksterslabRlinreg/index.html)
for package documentation.
