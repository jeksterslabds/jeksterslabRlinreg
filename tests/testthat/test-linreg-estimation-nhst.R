#' ---
#' title: "Tests: The Linear Regression Model (nhst)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (nhst)}
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
# The Linear Regression Model {#linreg-estimation-nhst-example}
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
#' ## `jeksterslabRlinreg::linreg()`
#'
#+
n <- nrow(X)
k <- ncol(X)
betahat <- betahat(
  X = X,
  y = y
)
vcovbetahat <- vcovbetahat(
  X = X,
  y = y
)
se <- sqrt(diag(vcovbetahat))
result1 <- nhst(
  betahat = betahat,
  se = se,
  n = n,
  k = k
)
result2 <- betahatinference(
  X = X,
  y = y
)
result_coef1 <- as.vector(result1[, "coef"])
result_se1 <- as.vector(result1[, "se"])
result_t1 <- as.vector(result1[, "t"])
result_p1 <- as.vector(result1[, "p"])
result_coef2 <- as.vector(result2[, "coef"])
result_se2 <- as.vector(result2[, "se"])
result_t2 <- as.vector(result2[, "t"])
result_p2 <- as.vector(result2[, "p"])
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_coef <- summary(lmobj)[["coefficients"]][, "Estimate"]
lm_se <- summary(lmobj)[["coefficients"]][, "Std. Error"]
lm_t <- summary(lmobj)[["coefficients"]][, "t value"]
lm_p <- summary(lmobj)[["coefficients"]][, "Pr(>|t|)"]
#'
#'
#+
context("Test linreg-estimation-linreg")
test_that("coef.", {
  expect_equivalent(
    length(lm_coef),
    length(result_coef1),
    length(result_coef2)
  )
  for (i in seq_along(result_coef1)) {
    expect_equivalent(
      result_coef1[i],
      lm_coef[i]
    )
    expect_equivalent(
      result_coef2[i],
      lm_coef[i]
    )
  }
})
test_that("se.", {
  expect_equivalent(
    length(lm_se),
    length(result_se1),
    length(result_se2)
  )
  for (i in seq_along(result_se1)) {
    expect_equivalent(
      result_se1[i],
      lm_se[i]
    )
    expect_equivalent(
      result_se2[i],
      lm_se[i]
    )
  }
})
test_that("t.", {
  expect_equivalent(
    length(lm_t),
    length(result_t1),
    length(result_t2)
  )
  for (i in seq_along(result_t1)) {
    expect_equivalent(
      result_t1[i],
      lm_t[i]
    )
    expect_equivalent(
      result_t2[i],
      lm_t[i]
    )
  }
})
test_that("p.", {
  expect_equivalent(
    length(lm_p),
    length(result_p1),
    length(result_p2)
  )
  for (i in seq_along(result_p1)) {
    expect_equivalent(
      result_p1[i],
      lm_p[i]
    )
    expect_equivalent(
      result_p2[i],
      lm_p[i]
    )
  }
})
#'
