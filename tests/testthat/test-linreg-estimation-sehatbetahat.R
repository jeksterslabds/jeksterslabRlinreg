#' ---
#' title: "Tests: The Linear Regression Model (Standard Errors of Estimates of Regression Coefficients)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Standard Errors of Estimates of Regression Coefficients)}
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
# The Linear Regression Model: Standard Errors of Estimates of Regression Coefficients {#linreg-estimation-se-example}
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
#' ## Standard Errors of Estimates of Regression Coefficients
#'
#+
vcovhatbetahat <- vcovhatbetahat(
  X = X,
  y = y
)
vcovhatbetahatbiased <- vcovhatbetahatbiased(
  X = X,
  y = y
)
result_sehatbetahat1 <- .sehatbetahat(
  vcovhatbetahat = vcovhatbetahat
)
result_sehatbetahat2 <- .sehatbetahat(
  X = X,
  y = y
)
result_sehatbetahat3 <- sehatbetahat(
  X = X,
  y = y
)
#'
#' ## Biased Standard Errors of Estimates of Regression Coefficients
#'
#+
result_sehatbetahatbiased1 <- .sehatbetahatbiased(
  vcovhatbetahatbiased = vcovhatbetahatbiased
)
result_sehatbetahatbiased2 <- .sehatbetahatbiased(
  X = X,
  y = y
)
result_sehatbetahatbiased3 <- sehatbetahatbiased(
  X = X,
  y = y
)
#'
#' ## Standard Errors of Scaled Estimates of Regression Coefficients (textbook)
#'
#+
slopes <- slopes(
  X = X,
  y = y
)
slopesprime <- slopesprime(
  X = X,
  y = y
)
sehatbetahat <- as.vector(
  sehatbetahat(
    X = X,
    y = y
  )
)
sehatslopes <- sehatbetahat[-1]
result_sehatslopesprimetb1 <- .sehatslopesprimetb(
  slopes = slopes,
  sehatslopes = sehatslopes,
  slopesprime = slopesprime
)
result_sehatslopesprimetb2 <- .sehatslopesprimetb(
  sehatslopes = sehatslopes,
  slopesprime = slopesprime,
  X = X,
  y = y
)
result_sehatslopesprimetb3 <- .sehatslopesprimetb(
  slopes = slopes,
  slopesprime = slopesprime,
  X = X,
  y = y
)
result_sehatslopesprimetb4 <- .sehatslopesprimetb(
  sehatslopes = sehatslopes,
  slopesprime = slopesprime,
  X = X,
  y = y
)
result_sehatslopesprimetb5 <- .sehatslopesprimetb(
  X = X,
  y = y
)
result_sehatslopesprimetb6 <- sehatslopesprimetb(
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
lm_se <- as.vector(sqrt(diag(vcov(lmobj))))
#'
#'
#'
#' ## `lm()` function - scaled data
#'
#+
lmscaledobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = as.data.frame(scale(jeksterslabRdatarepo::wages))
)
lmscaled_se <- as.vector(summary(lmscaledobj)[["coefficients"]][, "Std. Error"])
lmscaled_se <- lmscaled_se[-1]
#'
#+ testthat_unstandardized
result_sehatbetahat1 <- as.vector(result_sehatbetahat1)
result_sehatbetahat2 <- as.vector(result_sehatbetahat2)
result_sehatbetahat3 <- as.vector(result_sehatbetahat3)
result_sehatbetahatbiased1 <- as.vector(result_sehatbetahatbiased1)
result_sehatbetahatbiased2 <- as.vector(result_sehatbetahatbiased2)
result_sehatbetahatbiased3 <- as.vector(result_sehatbetahatbiased3)
context("Test linreg-estimation-vcov")
test_that("unbiased", {
  for (i in 1:length(lm_se)) {
    expect_equivalent(
      result_sehatbetahat1[i],
      lm_se[i]
    )
    expect_equivalent(
      result_sehatbetahat2[i],
      lm_se[i]
    )
    expect_equivalent(
      result_sehatbetahat3[i],
      lm_se[i]
    )
  }
})
test_that("biased", {
  for (i in 1:length(lm_se)) {
    expect_equivalent(
      round(result_sehatbetahatbiased1[i], digits = 1),
      round(lm_se[i], digits = 1)
    )
    expect_equivalent(
      round(result_sehatbetahatbiased2[i], digits = 1),
      round(lm_se[i], digits = 1)
    )
    expect_equivalent(
      round(result_sehatbetahatbiased3[i], digits = 1),
      round(lm_se[i], digits = 1)
    )
  }
})
#'
#+ testthat_scaled
result_sehatslopesprimetb1 <- as.vector(result_sehatslopesprimetb1)
result_sehatslopesprimetb2 <- as.vector(result_sehatslopesprimetb2)
result_sehatslopesprimetb3 <- as.vector(result_sehatslopesprimetb3)
result_sehatslopesprimetb4 <- as.vector(result_sehatslopesprimetb4)
result_sehatslopesprimetb5 <- as.vector(result_sehatslopesprimetb5)
result_sehatslopesprimetb6 <- as.vector(result_sehatslopesprimetb6)
test_that("sehatslopesprimetb", {
  for (i in 1:length(lmscaled_se)) {
    expect_equivalent(
      result_sehatslopesprimetb1[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopesprimetb2[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopesprimetb3[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopesprimetb4[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopesprimetb5[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopesprimetb6[i],
      lmscaled_se[i]
    )
  }
})
