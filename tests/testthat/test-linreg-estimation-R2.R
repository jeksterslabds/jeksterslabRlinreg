#' ---
#' title: "Tests: The Linear Regression Model (Coefficient of Determination)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Coefficient of Determination)}
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
# The Linear Regression Model: Coefficient of Determination {#linreg-estimation-R2-example}
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
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
head(X)
head(y)
#'
#' ## $R^{2}$
#'
#+
RSS <- RSS(
  X = X,
  y = y
)
ESS <- ESS(
  X = X,
  y = y
)
TSS <- TSS(
  y = y
)
epsilonhat <- epsilonhat(
  X = X,
  y = y
)
betahat <- betahat(
  X = X,
  y = y
)
yhat <- yhat(
  X = X,
  y = y
)
ybar <- mean(y)
R2 <- R2(
  X = X,
  y = y
)
n <- nrow(X)
k <- ncol(X)
result_r21 <- .R2fromRSS(
  RSS = RSS,
  TSS = NULL,
  epsilonhat = NULL,
  X = NULL,
  y = y,
  betahat = NULL
)
result_r22 <- .R2fromRSS(
  RSS = NULL,
  TSS = TSS,
  epsilonhat = epsilonhat,
  X = NULL,
  y = NULL,
  betahat = NULL
)
result_r23 <- .R2fromRSS(
  RSS = NULL,
  TSS = TSS,
  epsilonhat = epsilonhat,
  X = NULL,
  y = NULL,
  betahat = betahat
)
result_r24 <- .R2fromRSS(
  RSS = RSS,
  TSS = TSS,
  epsilonhat = NULL,
  X = NULL,
  y = NULL,
  betahat = NULL
)
result_r25 <- .R2fromRSS(
  RSS = NULL,
  TSS = NULL,
  epsilonhat = NULL,
  X = X,
  y = y,
  betahat = NULL
)
result_r26 <- .R2fromESS(
  ESS = ESS,
  TSS = NULL,
  X = NULL,
  y = y,
  yhat = NULL,
  ybar = NULL,
  betahat = NULL
)
result_r27 <- .R2fromESS(
  ESS = NULL,
  TSS = NULL,
  X = NULL,
  y = y,
  yhat = yhat,
  ybar = NULL,
  betahat = NULL
)
result_r28 <- .R2fromESS(
  ESS = NULL,
  TSS = NULL,
  X = X,
  y = y,
  yhat = NULL,
  ybar = ybar,
  betahat = NULL
)
result_r29 <- .R2fromESS(
  ESS = NULL,
  TSS = NULL,
  X = X,
  y = y,
  yhat = NULL,
  ybar = ybar,
  betahat = betahat
)
result_r210 <- R2(
  X = X,
  y = y,
  fromRSS = TRUE
)
result_r211 <- R2(
  X = X,
  y = y,
  fromRSS = FALSE
)
#'
#' ## $\bar{R}^{2}$
#'
#+
result_rbar21 <- .Rbar2(
  R2 = NULL,
  n,
  k,
  X,
  y,
  fromRSS = TRUE
)
result_rbar21 <- .Rbar2(
  R2 = R2,
  n = n,
  k = k,
  X = NULL,
  y = NULL,
  fromRSS = TRUE
)
result_rbar22 <- .Rbar2(
  R2 = R2,
  n = n,
  k = k,
  X = NULL,
  y = NULL,
  fromRSS = FALSE
)
result_rbar23 <- .Rbar2(
  R2 = NULL,
  n = NULL,
  k = NULL,
  X = X,
  y = y,
  fromRSS = TRUE
)
result_rbar24 <- .Rbar2(
  R2 = NULL,
  n = NULL,
  k = NULL,
  X = X,
  y = y,
  fromRSS = FALSE
)
result_rbar25 <- Rbar2(
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
lm_r2 <- summary(lmobj)$r.squared
lm_rbar2 <- summary(lmobj)$adj.r.squared
#'
#'
#+
result_r2 <- c(
  result_r21, result_r22, result_r23, result_r24, result_r25,
  result_r26, result_r27, result_r28, result_r29, result_r210, result_r211
)
result_rbar2 <- c(
  result_rbar21, result_rbar22, result_rbar23,
  result_rbar24, result_rbar25
)
context("Test linreg-estimation-R2.")
test_that("R2", {
  for (i in seq_along(result_r2)) {
    expect_equivalent(
      lm_r2,
      result_r2[i]
    )
  }
})
test_that("Rbar2", {
  for (i in seq_along(result_rbar2)) {
    expect_equivalent(
      lm_rbar2,
      result_rbar2[i]
    )
  }
})
