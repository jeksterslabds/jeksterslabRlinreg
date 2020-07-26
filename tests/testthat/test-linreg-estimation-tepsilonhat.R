#' ---
#' title: "Tests: The Linear Regression Model (Studentized Residuals)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Studentized Residuals)}
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
# The Linear Regression Model: Studentized Residuals {#linreg-estimation-tepsilonhat-example}
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
#' ## Studentized Residuals
#'
#+
h <- h(X = X)
sigma2epsilonhat <- sigma2epsilonhat(
  X = X,
  y = y
)
epsilonhat <- epsilonhat(
  X = X,
  y = y
)
result_tepsilonhat1 <- as.vector(
  .tepsilonhat(
    sigma2epsilonhat = sigma2epsilonhat,
    h = h,
    epsilonhat = epsilonhat
  )
)
result_tepsilonhat2 <- as.vector(
  tepsilonhat(
    X = X,
    y = y
  )
)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_tepsilonhat <- as.vector(rstudent(lmobj))
#'
#'
#+
context("Test linreg-estimation-tepsilonhat")
test_that("result_tepsilonhat1.", {
  expect_equivalent(
    length(result_tepsilonhat1),
    length(lm_tepsilonhat)
  )
  for (i in seq_along(result_tepsilonhat1)) {
    expect_equivalent(
      round(result_tepsilonhat1[i], digits = 0),
      round(lm_tepsilonhat[i], digits = 0)
    )
  }
})
test_that("result_tepsilonhat2.", {
  expect_equivalent(
    length(result_tepsilonhat2),
    length(lm_tepsilonhat)
  )
  for (i in seq_along(result_tepsilonhat2)) {
    expect_equivalent(
      round(result_tepsilonhat2[i], digits = 0),
      round(lm_tepsilonhat[i], digits = 0)
    )
  }
})
