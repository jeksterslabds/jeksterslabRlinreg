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
library(testthat)
library(jeksterslabRlinreg)
#'
#' ## Data
#'
#' See `jeksterslabRdatarepo::wages()` for the data set used in this example.
#'
#+
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
X <- X[, -ncol(X)]
X <- cbind(
  Intercept = 1,
  X
)
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
result_sigma2epsilonhat1 <- .sigma2epsilonhat(
  RSS = RSS,
  n = n,
  k = k
)
result_sigma2epsilonhat2 <- .sigma2epsilonhat(
  RSS = NULL,
  n = n,
  k = k,
  X = X,
  y = y
)
result_sigma2epsilonhat3 <- sigma2epsilonhat(
  X = X,
  y = y
)
#'
#' ## Residual Variance (Biased)
#'
#+
result_sigma2epsilonhatbiased1 <- .sigma2epsilonhatbiased(
  RSS = RSS,
  n = n
)
result_sigma2epsilonhatbiased2 <- .sigma2epsilonhatbiased(
  RSS = NULL,
  n = n,
  X = X,
  y = y
)
result_sigma2epsilonhatbiased3 <- sigma2epsilonhatbiased(
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
lm_sigma2epsilonhat <- summary(lmobj)$sigma^2
lm_anova <- anova(lmobj)
lm_RSS <- lm_anova["Residuals", "Sum Sq"]
lm_sigma2epsilonhatbiased <- lm_RSS / n
#'
#'
#+
sigma2epsilonhat <- c(
  result_sigma2epsilonhat1, result_sigma2epsilonhat2, result_sigma2epsilonhat3
)
sigma2epsilonhatbiased <- c(
  result_sigma2epsilonhatbiased1, result_sigma2epsilonhatbiased2, result_sigma2epsilonhatbiased3
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
test_that("sigma2epsilonhatbiased", {
  for (i in seq_along(sigma2epsilonhatbiased)) {
    expect_equivalent(
      lm_sigma2epsilonhatbiased,
      sigma2epsilonhatbiased[i]
    )
  }
})
