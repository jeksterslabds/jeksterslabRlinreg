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
slopeshat <- slopeshat(
  X = X,
  y = y
)
slopeshatprime <- slopeshatprime(
  X = X,
  y = y
)
sehatbetahat <- as.vector(
  sehatbetahat(
    X = X,
    y = y
  )
)
sehatslopeshat <- sehatbetahat[-1]
result_sehatslopeshatprimetb1 <- .sehatslopeshatprimetb(
  slopeshat = slopeshat,
  sehatslopeshat = sehatslopeshat,
  slopeshatprime = slopeshatprime
)
result_sehatslopeshatprimetb2 <- .sehatslopeshatprimetb(
  sehatslopeshat = sehatslopeshat,
  slopeshatprime = slopeshatprime,
  X = X,
  y = y
)
result_sehatslopeshatprimetb3 <- .sehatslopeshatprimetb(
  slopeshat = slopeshat,
  slopeshatprime = slopeshatprime,
  X = X,
  y = y
)
result_sehatslopeshatprimetb4 <- .sehatslopeshatprimetb(
  sehatslopeshat = sehatslopeshat,
  slopeshatprime = slopeshatprime,
  X = X,
  y = y
)
result_sehatslopeshatprimetb5 <- .sehatslopeshatprimetb(
  X = X,
  y = y
)
result_sehatslopeshatprimetb6 <- sehatslopeshatprimetb(
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
result_sehatslopeshatprimetb1 <- as.vector(result_sehatslopeshatprimetb1)
result_sehatslopeshatprimetb2 <- as.vector(result_sehatslopeshatprimetb2)
result_sehatslopeshatprimetb3 <- as.vector(result_sehatslopeshatprimetb3)
result_sehatslopeshatprimetb4 <- as.vector(result_sehatslopeshatprimetb4)
result_sehatslopeshatprimetb5 <- as.vector(result_sehatslopeshatprimetb5)
result_sehatslopeshatprimetb6 <- as.vector(result_sehatslopeshatprimetb6)
test_that("sehatslopeshatprimetb", {
  for (i in 1:length(lmscaled_se)) {
    expect_equivalent(
      result_sehatslopeshatprimetb1[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopeshatprimetb2[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopeshatprimetb3[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopeshatprimetb4[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopeshatprimetb5[i],
      lmscaled_se[i]
    )
    expect_equivalent(
      result_sehatslopeshatprimetb6[i],
      lmscaled_se[i]
    )
  }
})
