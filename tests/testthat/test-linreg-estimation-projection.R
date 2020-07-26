#' ---
#' title: "Tests: The Linear Regression Model (Projection Matrix)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Projection Matrix)}
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
# The Linear Regression Model: Projection Matrix {#linreg-estimation-projection-example}
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
#' ## Projection Matrix / Hat Matrix
#'
#+
Pmatrix <- P(X = X)
result_yhat <- as.vector(Pmatrix %*% y)
#'
#' ## Residual Maker Matrix
#'
#+
Mmatrix1 <- M(X = X)
Mmatrix2 <- .M(X = X, P = Pmatrix)
result_epsilonhat1 <- as.vector(Mmatrix1 %*% y)
result_epsilonhat2 <- as.vector(Mmatrix2 %*% y)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_yhat <- as.vector(predict(lmobj))
lm_epsilonhat <- as.vector(residuals(lmobj))
#'
#'
#+
context("Test linreg-estimation-projection")
test_that("Py = yhat.", {
  expect_equivalent(
    length(result_yhat),
    length(lm_yhat)
  )
  for (i in seq_along(result_yhat)) {
    expect_equivalent(
      result_yhat[i],
      lm_yhat[i]
    )
  }
})
test_that("My = epsilonhat.", {
  expect_equivalent(
    length(result_epsilonhat1),
    length(result_epsilonhat2),
    length(lm_epsilonhat)
  )
  for (i in seq_along(result_epsilonhat1)) {
    expect_equivalent(
      result_epsilonhat1[i],
      lm_epsilonhat[i]
    )
  }
  for (i in seq_along(result_epsilonhat2)) {
    expect_equivalent(
      result_epsilonhat2[i],
      lm_epsilonhat[i]
    )
  }
})
