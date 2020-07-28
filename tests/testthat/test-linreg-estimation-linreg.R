#' ---
#' title: "Tests: The Linear Regression Model"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ include = FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#'
# The Linear Regression Model {#linreg-estimation-linreg-example}
#'
#+ echo = FALSE
library(testthat)
library(jeksterslabRlinreg)
library(lavaan)
#'
#' ## Data
#'
#' See `jeksterslabRdatarepo::wages.matrix()` for the data set used in this example.
#'
#+
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
# age is removed
X <- X[, -ncol(X)]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
head(X)
head(y)
#'
#'
#' ## Data
#'
#' In this example,
#' we are interested in predictors of wages.
#' The regressor variables are gender, race, union membership, education, and work experience.
#' The regressand variable is hourly wage in US dollars.
#'
#' See `jeksterslabRdatarepo::wages.matrix()` for the data set used in this example.
#'
#+
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
# age is removed
X <- X[, -ncol(X)]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
head(X)
head(y)
#'
#'
#' ## `jeksterslabRlinreg::linreg()`
#'
#' The `jeksterslabRlinreg::linreg()` function
#' fits a linear regression model using `X` and `y`.
#' In this example, `X` consists of a column of constants,
#' `gender`, `race`, `union` membership, `education`, and work `experience`.
#' and `y` consists of hourly `wages` in US dollars.
#'
#' The output includes the following:
#'
#' - Model assessment
#' - ANOVA table
#' - Table of regression coefficients with the following columns
#'   - Regression coefficients
#'   - Standard errors
#'   - $t$ statistic
#'   - $p$ value
#'   - Standardized coefficients
#' - Confidence intervals (0.05, 0.5, 2.5, 97.5, 99.5, 99.95)
#' - Means and standard deviations
#' - Scatterplot matrix
#' - Residual plots
#'
#' ## Using Unbiased Standard Errors
#' 
#+
linreg(
  X = X,
  y = y
)
#'
#' ## Using Biased Standard Errors
#' 
#+
linreg(
  X = X,
  y = y,
  ubiased = FALSE
)
#'
#' ## `lm()` function
#'
#' The `lm()` function is the default option for fitting a linear model in `R`.
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
summary(lmobj)
#'
#' ## `lavaan::sem()` function
#' 
#' Linear regression in SEM
#' 
#+
model <- c(
  wages ~ gender + race + union + education + experience
)
#' 
#' ### Wishart Likelihood (Unbiased)
#' 
#+
lavobj <- lavaan::sem(
  model = model,
  data = jeksterslabRdatarepo::wages,
  likelihood = "wishart"
)
summary(lavobj)
#' 
#' ### Normal Likelihood (Biased)
#' 
#+
lavobj <- lavaan::sem(
  model = model,
  data = jeksterslabRdatarepo::wages,
  likelihood = "normal"
)
summary(lavobj)
