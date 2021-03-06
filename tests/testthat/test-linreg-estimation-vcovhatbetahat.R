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
sigma2hatepsilonhat <- sigma2hatepsilonhat(
  X = X,
  y = y
)
result_vcovhatbetahat1 <- .vcovhatbetahat(
  sigma2hatepsilonhat = sigma2hatepsilonhat,
  X = X
)
result_vcovhatbetahat1 <- as.vector(result_vcovhatbetahat1)
result_vcovhatbetahat2 <- .vcovhatbetahat(
  X = X,
  y = y
)
result_vcovhatbetahat2 <- as.vector(result_vcovhatbetahat2)
result_vcovhatbetahat3 <- vcovhatbetahat(
  X = X,
  y = y
)
result_vcovhatbetahat3 <- as.vector(result_vcovhatbetahat3)
#'
#' ## Variance-Covariance Matrix of Estimates of Regression Coefficients (biased)
#'
#+
sigma2hatepsilonhatbiased <- sigma2hatepsilonhatbiased(
  X = X,
  y = y
)
result_vcovhatbetahatbiased1 <- .vcovhatbetahatbiased(
  sigma2hatepsilonhatbiased = sigma2hatepsilonhatbiased,
  X = X
)
result_vcovhatbetahatbiased1 <- as.vector(result_vcovhatbetahatbiased1)
result_vcovhatbetahatbiased2 <- .vcovhatbetahatbiased(
  X = X,
  y = y
)
result_vcovhatbetahatbiased2 <- as.vector(result_vcovhatbetahatbiased2)
result_vcovhatbetahatbiased3 <- vcovhatbetahatbiased(
  X = X,
  y = y
)
result_vcovhatbetahatbiased3 <- as.vector(result_vcovhatbetahatbiased3)
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
      result_vcovhatbetahat1[i],
      lm_vcov[i]
    )
    expect_equivalent(
      result_vcovhatbetahat1[i],
      lm_vcov[i]
    )
    expect_equivalent(
      result_vcovhatbetahat1[i],
      lm_vcov[i]
    )
  }
})
test_that("biased", {
  for (i in 1:length(lm_vcov)) {
    expect_equivalent(
      round(result_vcovhatbetahatbiased1[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
    expect_equivalent(
      round(result_vcovhatbetahatbiased1[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
    expect_equivalent(
      round(result_vcovhatbetahatbiased1[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
  }
})
