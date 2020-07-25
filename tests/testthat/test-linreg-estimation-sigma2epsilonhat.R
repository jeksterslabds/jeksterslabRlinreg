#' ---
#' title: "Tests: The Linear Regression Model (Residual Variance)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Residual Variance)}
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
# The Linear Regression Model: Residual Variance {#linreg-estimation-sigma2epsilonhat-example}
#'
#+ echo = FALSE
library(microbenchmark)
library(testthat)
library(jeksterslabRlinreg)
#'
#' ## Data
#'
#' See `jeksterslabRdatarepo::wages()` for the data set used in this example.
#'
#+
varnames <- c(
  "wages", "gender", "race", "union", "education", "experience"
)
Xvars <- c(
  "gender", "race", "union", "education", "experience"
)
wages <- jeksterslabRdatarepo::wages
wages <- wages[, varnames]
X <- wages[, Xvars]
X <- cbind(Intercept = 1, X)
X <- as.matrix(X)
y <- wages[, "wages"]
head(X)
head(y)
#'
#' ## Residual Variance
#'
#+
n <- nrow(X)
k <- ncol(X)
betahat <- betahat(
  X = X,
  y = y
)
epsilonhat <- epsilonhat(
  X = X,
  y = y
)
RSS <- RSS(
  X = X,
  y = y
)
result_sigma2epsilonhat1 <- .sigma2epsilonhat(
  RSS = RSS,
  n = n,
  k = k,
  type = "unbiased"
)
result_sigma2epsilonhat2 <- .sigma2epsilonhat(
  RSS = NULL,
  n = n,
  k = k,
  type = "unbiased",
  epsilonhat = epsilonhat
)
result_sigma2epsilonhat3 <- .sigma2epsilonhat(
  RSS = NULL,
  n = n,
  k = k,
  type = "unbiased",
  epsilonhat = NULL,
  X = X,
  y = y,
  betahat = betahat
)
result_sigma2epsilonhat4 <- .sigma2epsilonhat(
  RSS = NULL,
  n = n,
  k = k,
  type = "unbiased",
  epsilonhat = NULL,
  X = X,
  y = y,
  betahat = NULL
)
result_sigma2epsilonhat5 <- sigma2epsilonhat(
  X = X,
  y = y,
  type = "unbiased"
)
result_sigma2epsilonhat6 <- sigma2epsilonhat(
  X = X,
  y = y,
  type = "both"
)
result_sigma2epsilonhat6 <- result_sigma2epsilonhat6[[1]]
biased <- sigma2epsilonhat(
  X = X,
  y = y,
  type = "biased"
)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_sigma2epsilonhat <- summary(lmobj)$sigma^2
#'
#'
#+
sigma2epsilonhat <- c(
  result_sigma2epsilonhat1, result_sigma2epsilonhat2, result_sigma2epsilonhat3,
  result_sigma2epsilonhat4, result_sigma2epsilonhat5, result_sigma2epsilonhat6
)
context("Test linreg-estimation-sigma2epsilonhat.")
test_that("sigma2epsilonhat", {
  for (i in seq_along(sigma2epsilonhat)) {
    expect_equivalent(
      lm_sigma2epsilonhat,
      sigma2epsilonhat[i]
    )
  }
})
