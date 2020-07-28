#' ---
#' title: "Tests: The Linear Regression Model (Model Assessment)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (Model Assessment)}
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
# The Linear Regression Model: Model Assessment {#linreg-estimation-MSE-example}
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
#'
#' ## Model Assessment
#'
#+
RSS <- RSS(
  X = X,
  y = y
)
TSS <- TSS(
  y = y
)
n <- nrow(X)
k <- ncol(X)
result1 <- .model(
  RSS = RSS,
  TSS = TSS,
  n = n,
  k = k
)
result2 <- .model(
  RSS = NULL,
  TSS = TSS,
  X = X,
  y = y
)
result3 <- .model(
  RSS = RSS,
  TSS = NULL,
  n = n,
  k = k,
  X = X,
  y = y
)
result4 <- .model(
  X = X,
  y = y
)
result5 <- model(
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
lm_RSS <- sum(lmobj$residuals^2)
lm_MSE <- mean(lmobj$residuals^2)
lm_RMSE <- sqrt(lm_MSE)
lm_R2 <- summary(lmobj)$r.squared
lm_Rbar2 <- summary(lmobj)$adj.r.squared
#'
#'
#+
result_RSS <- c(
  result1["RSS"],
  result2["RSS"],
  result3["RSS"],
  result4["RSS"],
  result5["RSS"]
)
result_MSE <- c(
  result1["MSE"],
  result2["MSE"],
  result3["MSE"],
  result4["MSE"],
  result5["MSE"]
)
result_RMSE <- c(
  result1["RMSE"],
  result2["RMSE"],
  result3["RMSE"],
  result4["RMSE"],
  result5["RMSE"]
)
result_R2 <- c(
  result1["R2"],
  result2["R2"],
  result3["R2"],
  result4["R2"],
  result5["R2"]
)
result_Rbar2 <- c(
  result1["Rbar2"],
  result2["Rbar2"],
  result3["Rbar2"],
  result4["Rbar2"],
  result5["Rbar2"]
)
context("Test linreg-estimation-model.")
test_that("RSS", {
  for (i in seq_along(result_RSS)) {
    expect_equivalent(
      lm_RSS,
      result_RSS[i]
    )
  }
})
test_that("MSE", {
  for (i in seq_along(result_MSE)) {
    expect_equivalent(
      lm_MSE,
      result_MSE[i]
    )
  }
})
test_that("RMSE", {
  for (i in seq_along(result_RMSE)) {
    expect_equivalent(
      lm_RMSE,
      result_RMSE[i]
    )
  }
})
test_that("R2", {
  for (i in seq_along(result_R2)) {
    expect_equivalent(
      lm_R2,
      result_R2[i]
    )
  }
})
test_that("Rbar2", {
  for (i in seq_along(result_Rbar2)) {
    expect_equivalent(
      lm_Rbar2,
      result_Rbar2[i]
    )
  }
})
