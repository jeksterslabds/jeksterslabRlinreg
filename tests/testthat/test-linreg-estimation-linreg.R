#' ---
#' title: "Tests: The Linear Regression Model"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model}
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
# The Linear Regression Model {#linreg-estimation-linreg-example}
#'
#+ echo = FALSE
library(microbenchmark)
library(testthat)
library(jeksterslabRlinreg)
#'
#' ## Data
#'
#' See `jeksterslabRdatarepo::wages()` for the data set used in this example.
#'
#+
varnames <- c(
  "wages", "gender", "race", "union", "education", "experience"
)
Xvars <- c(
  "gender", "race", "union", "education", "experience"
)
wages <- jeksterslabRdatarepo::wages
wages <- wages[, varnames]
X <- wages[, Xvars]
X <- cbind(Intercept = 1, X)
X <- as.matrix(X)
y <- wages[, "wages"]
head(X)
head(y)
#'
#' ## `jeksterslabRlinreg::linreg()`
#'
#+
result_linreg <- linreg(
  X = X,
  y = y
)
str(result_linreg)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_betahat <- as.vector(coef(lmobj))
lm_yhat <- as.vector(predict(lmobj))
lm_epsilonhat <- as.vector(residuals(lmobj))
lm_anova <- anova(lmobj)
lm_RSS <- lm_anova["Residuals", "Sum Sq"]
lm_TSS <- sum(lm_anova[["Sum Sq"]])
lm_ESS <- lm_TSS - lm_RSS
summary(lmobj)
#'
#'
#+
context("Test linreg-estimation-linreg")
result_betahat <- as.vector(result_linreg[["betahat"]])
result_yhat <- as.vector(result_linreg[["yhat"]])
result_epsilonhat <- as.vector(result_linreg[["epsilonhat"]])
result_RSS <- as.vector(result_linreg[["RSS"]])
result_ESS <- as.vector(result_linreg[["ESS"]])
result_TSS <- as.vector(result_linreg[["TSS"]])
test_that("betahat.", {
  expect_equivalent(
    length(result_betahat),
    length(lm_betahat)
  )
  for (i in seq_along(result_betahat)) {
    expect_equivalent(
      result_betahat[i],
      lm_betahat[i]
    )
  }
})
test_that("yhat.", {
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
test_that("epsilonhat.", {
  expect_equivalent(
    length(result_epsilonhat),
    length(lm_epsilonhat)
  )
  for (i in seq_along(result_epsilonhat)) {
    expect_equivalent(
      result_epsilonhat[i],
      lm_epsilonhat[i]
    )
  }
})
test_that("RSS", {
  expect_equivalent(
    lm_RSS,
    result_RSS
  )
})
test_that("ESS", {
  expect_equivalent(
    lm_ESS,
    result_ESS
  )
})
test_that("TSS", {
  expect_equivalent(
    lm_TSS,
    result_TSS
  )
})
#'