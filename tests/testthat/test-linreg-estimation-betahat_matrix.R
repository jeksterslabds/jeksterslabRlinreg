#' ---
#' title: "Tests: Estimates of Regression Coefficients from summary matrices"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: Estimates of Regression Coefficients from summary matrices}
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
# Estimates of Regression Coefficients from summary matrices {#linreg-estimation-betahat_matrix}
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
#' ## Unstandardized
#'
#+
data <- X
data[, 1] <- y
Sigmahat <- cov(data)
SigmaXhat <- Sigmahat[-1, -1]
sigmayX <- as.vector(Sigmahat[, 1])
sigmayX <- sigmayX[-1]
result1_slopes <- .slopeshat(
  SigmaXhat = SigmaXhat,
  sigmayX = sigmayX
)
result2_slopes <- .slopeshat(
  X = X,
  y = y
)
result3_slopes <- slopeshat(
  X = X,
  y = y
)
result1_intercept <- .intercepthat(
  slopeshat = result1_slopes,
  muyhat = mean(y),
  muXhat = colMeans(X[, -1])
)
result2_intercept <- .intercepthat(
  X = X,
  y = y
)
result3_intercept <- .intercepthat(
  X = X,
  y = y
)
result1_betahat <- c(result1_intercept, as.vector(result1_slopes))
result2_betahat <- c(result2_intercept, as.vector(result2_slopes))
result3_betahat <- c(result3_intercept, as.vector(result3_slopes))
#'
#' ## Standardized
#'
#+
Rhat <- cor(data)
RXhat <- Rhat[-1, -1]
ryXhat <- as.vector(Rhat[, 1])
ryXhat <- ryXhat[-1]
result1_std.slopes <- .slopeshatprime(
  X = X,
  y = y
)
result2_std.slopes <- .slopeshatprime(
  RXhat = RXhat,
  ryXhat = ryXhat
)
result3_std.slopes <- slopeshatprime(
  X = X,
  y = y
)
result1_betahatprime <- c(0, as.vector(result1_std.slopes))
result2_betahatprime <- c(0, as.vector(result2_std.slopes))
result3_betahatprime <- c(0, as.vector(result3_std.slopes))
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
lmscaledobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = as.data.frame(scale(jeksterslabRdatarepo::wages))
)
lm_betahat <- coef(lmobj)
lm_betahatprime <- coef(lmscaledobj)
#'
#+ testthat_unstandardized
context("Test Coefficients")
test_that("unstandardized.", {
  expect_equivalent(
    length(lm_betahat),
    length(result1_betahat),
    length(result2_betahat),
    length(result3_betahat)
  )
  for (i in seq_along(result1_betahat)) {
    expect_equivalent(
      lm_betahat[i],
      result1_betahat[i]
    )
    expect_equivalent(
      lm_betahat[i],
      result2_betahat[i]
    )
    expect_equivalent(
      lm_betahat[i],
      result3_betahat[i]
    )
  }
})
test_that("standardized.", {
  expect_equivalent(
    length(lm_betahatprime),
    length(result1_betahatprime),
    length(result2_betahatprime),
    length(result3_betahatprime)
  )
  for (i in seq_along(result1_betahatprime)) {
    expect_equivalent(
      lm_betahatprime[i],
      result1_betahatprime[i]
    )
    expect_equivalent(
      lm_betahatprime[i],
      result2_betahatprime[i]
    )
    expect_equivalent(
      lm_betahatprime[i],
      result3_betahatprime[i]
    )
  }
})
