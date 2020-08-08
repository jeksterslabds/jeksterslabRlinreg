#' ---
#' title: "Tests: The Linear Regression Model (Plots)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Plots)}
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
# The Linear Regression Model (Plots) {#linreg-plots}
#'
#+ echo = FALSE
library(testthat)
library(jeksterslabRlinreg)
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
#' ## Scatter Plot Matrix
#+
scatter.plot(
  X = X,
  y = y
)
#'
#' ## Residual Plot
#'
#+
residual.plot(
  X = X,
  y = y
)
