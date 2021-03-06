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
# The Linear Regression Model: Residual Variance {#linreg-estimation-sigma2hatepsilonhat-example}
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
#' ## Residual Variance
#'
#+
n <- nrow(X)
k <- ncol(X)
betahat <- betahat(
  X = X,
  y = y
)
RSS <- RSS(
  X = X,
  y = y
)
result_sigma2hatepsilonhat1 <- .sigma2hatepsilonhat(
  RSS = RSS,
  n = n,
  k = k
)
result_sigma2hatepsilonhat2 <- .sigma2hatepsilonhat(
  n = n,
  k = k,
  X = X,
  y = y
)
result_sigma2hatepsilonhat3 <- sigma2hatepsilonhat(
  X = X,
  y = y
)
#'
#' ## Residual Variance (Biased)
#'
#+
result_sigma2hatepsilonhatbiased1 <- .sigma2hatepsilonhatbiased(
  RSS = RSS,
  n = n
)
result_sigma2hatepsilonhatbiased2 <- .sigma2hatepsilonhatbiased(
  n = n,
  X = X,
  y = y
)
result_sigma2hatepsilonhatbiased3 <- sigma2hatepsilonhatbiased(
  X = X,
  y = y
)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_sigma2hatepsilonhat <- summary(lmobj)$sigma^2
lm_anova <- anova(lmobj)
lm_RSS <- lm_anova["Residuals", "Sum Sq"]
lm_sigma2hatepsilonhatbiased <- lm_RSS / n
#'
#'
#+
sigma2hatepsilonhat <- c(
  result_sigma2hatepsilonhat1, result_sigma2hatepsilonhat2, result_sigma2hatepsilonhat3
)
sigma2hatepsilonhatbiased <- c(
  result_sigma2hatepsilonhatbiased1, result_sigma2hatepsilonhatbiased2, result_sigma2hatepsilonhatbiased3
)
context("Test linreg-estimation-sigma2hatepsilonhat.")
test_that("sigma2hatepsilonhat", {
  for (i in seq_along(sigma2hatepsilonhat)) {
    expect_equivalent(
      lm_sigma2hatepsilonhat,
      sigma2hatepsilonhat[i]
    )
  }
})
test_that("sigma2hatepsilonhatbiased", {
  for (i in seq_along(sigma2hatepsilonhatbiased)) {
    expect_equivalent(
      lm_sigma2hatepsilonhatbiased,
      sigma2hatepsilonhatbiased[i]
    )
  }
})
