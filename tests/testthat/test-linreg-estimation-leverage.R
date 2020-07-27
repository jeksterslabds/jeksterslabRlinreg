#' ---
#' title: "Tests: The Linear Regression Model (Leverage)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Leverage)}
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
# The Linear Regression Model: Leverage {#linreg-estimation-leverage-example}
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
#' ## Leverage
#'
#+
result_h <- as.vector(h(X))
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_h <- as.vector(hatvalues(lmobj))
#'
#'
#+
context("Test linreg-estimation-leverage")
test_that("result_h.", {
  expect_equivalent(
    length(result_h),
    length(lm_h)
  )
  for (i in seq_along(result_h)) {
    expect_equivalent(
      round(result_h[i], digits = 0),
      round(lm_h[i], digits = 0)
    )
  }
})
