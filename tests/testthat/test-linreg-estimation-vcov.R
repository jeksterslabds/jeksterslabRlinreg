#' ---
#' title: "Tests: The Linear Regression Model (Variance-Covariance Matrix of Estimates of Regression Coefficients)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Variance-Covariance Matrix of Estimates of Regression Coefficients)}
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
# The Linear Regression Model: Variance-Covariance Matrix of Estimates of Regression Coefficients {#linreg-estimation-vcov-example}
#'
#+ echo = FALSE
library(testthat)
library(jeksterslabRlinreg)
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
#' ## Variance-Covariance Matrix of Estimates of Regression Coefficients
#'
#+
n <- nrow(X)
k <- ncol(X)
sigma2epsilonhat <- sigma2epsilonhat(
  X = X,
  y = y
)
result_vcovbetahat1 <- .vcovbetahat(
  sigma2epsilonhat = sigma2epsilonhat,
  X = X
)
result_vcovbetahat1 <- as.vector(result_vcovbetahat1)
result_vcovbetahat2 <- .vcovbetahat(
  X = X,
  y = y
)
result_vcovbetahat2 <- as.vector(result_vcovbetahat2)
result_vcovbetahat3 <- vcovbetahat(
  X = X,
  y = y
)
result_vcovbetahat3 <- as.vector(result_vcovbetahat3)
#'
#' ## Variance-Covariance Matrix of Estimates of Regression Coefficients (biased)
#'
#+
sigma2epsilonhatbiased <- sigma2epsilonhatbiased(
  X = X,
  y = y
)
result_vcovbetahatbiased1 <- .vcovbetahatbiased(
  sigma2epsilonhatbiased = sigma2epsilonhatbiased,
  X = X
)
result_vcovbetahatbiased1 <- as.vector(result_vcovbetahatbiased1)
result_vcovbetahatbiased2 <- .vcovbetahatbiased(
  X = X,
  y = y
)
result_vcovbetahatbiased2 <- as.vector(result_vcovbetahatbiased2)
result_vcovbetahatbiased3 <- vcovbetahatbiased(
  X = X,
  y = y
)
result_vcovbetahatbiased3 <- as.vector(result_vcovbetahatbiased3)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_vcov <- as.vector(vcov(lmobj))
#'
#'
#+
context("Test linreg-estimation-vcov")
test_that("unbiased", {
  for (i in 1:length(lm_vcov)) {
    expect_equivalent(
      result_vcovbetahat1[i],
      lm_vcov[i]
    )
    expect_equivalent(
      result_vcovbetahat1[i],
      lm_vcov[i]
    )
    expect_equivalent(
      result_vcovbetahat1[i],
      lm_vcov[i]
    )
  }
})
test_that("biased", {
  for (i in 1:length(lm_vcov)) {
    expect_equivalent(
      round(result_vcovbetahatbiased1[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
    expect_equivalent(
      round(result_vcovbetahatbiased1[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
    expect_equivalent(
      round(result_vcovbetahatbiased1[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
  }
})
