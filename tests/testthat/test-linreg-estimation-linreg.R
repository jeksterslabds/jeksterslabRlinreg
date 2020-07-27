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
result_linreg <- linreg(
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
lm_betahat <- as.vector(coef(lmobj))
lm_yhat <- as.vector(predict(lmobj))
lm_epsilonhat <- as.vector(residuals(lmobj))
lm_anova <- anova(lmobj)
lm_RSS <- lm_anova["Residuals", "Sum Sq"]
lm_TSS <- sum(lm_anova[["Sum Sq"]])
lm_ESS <- lm_TSS - lm_RSS
lm_R2 <- summary(lmobj)$r.squared
lm_Rbar2 <- summary(lmobj)$adj.r.squared
lm_vcov <- as.vector(vcov(lmobj))
lm_se <- as.vector(sqrt(diag(vcov(lmobj))))
lm_coef <- summary(lmobj)[["coefficients"]][, "Estimate"]
lm_se <- summary(lmobj)[["coefficients"]][, "Std. Error"]
lm_t <- summary(lmobj)[["coefficients"]][, "t value"]
lm_p <- summary(lmobj)[["coefficients"]][, "Pr(>|t|)"]
summary(lmobj)
#'
#+
context("Test linreg-estimation-linreg")
result_betahat <- as.vector(result_linreg[["betahat"]])
result_yhat <- as.vector(result_linreg[["yhat"]])
result_epsilonhat <- as.vector(result_linreg[["epsilonhat"]])
result_RSS <- result_linreg[["RSS"]]
result_ESS <- result_linreg[["ESS"]]
result_TSS <- result_linreg[["TSS"]]
result_R2 <- result_linreg[["R2"]]
result_Rbar2 <- result_linreg[["Rbar2"]]
result_vcov <- as.vector(result_linreg[["vcov"]])
result_vcovbiased <- as.vector(result_linreg[["vcovbiased"]])
result_se <- as.vector(result_linreg[["se"]])
result_sebiased <- as.vector(result_linreg[["sebiased"]])
result_t <- as.vector(result_linreg[["t"]])
result_p <- as.vector(result_linreg[["pt"]])
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
test_that("R2", {
  expect_equivalent(
    lm_R2,
    result_R2
  )
})
test_that("Rbar2", {
  expect_equivalent(
    lm_Rbar2,
    result_Rbar2
  )
})
test_that("vcov", {
  for (i in 1:length(lm_vcov)) {
    expect_equivalent(
      result_vcov[i],
      lm_vcov[i]
    )
  }
})
test_that("vcovbiased", {
  for (i in 1:length(lm_vcov)) {
    expect_equivalent(
      round(result_vcovbiased[i], digits = 1),
      round(lm_vcov[i], digits = 1)
    )
  }
})
test_that("se", {
  for (i in 1:length(lm_se)) {
    expect_equivalent(
      result_se[i],
      lm_se[i]
    )
  }
})
test_that("sebiased", {
  for (i in 1:length(lm_se)) {
    expect_equivalent(
      round(result_sebiased[i], digits = 1),
      round(lm_se[i], digits = 1)
    )
  }
})
test_that("t.", {
  expect_equivalent(
    length(lm_t),
    length(result_t)
  )
  for (i in seq_along(result_t)) {
    expect_equivalent(
      result_t[i],
      lm_t[i]
    )
  }
})
test_that("p.", {
  expect_equivalent(
    length(lm_p),
    length(result_p)
  )
  for (i in seq_along(result_p)) {
    expect_equivalent(
      result_p[i],
      lm_p[i]
    )
  }
})
#'
#'
#+ echo = FALSE
# coverage
invisible(
  out <- linreg(
    X = X,
    y = y,
    unbiased = FALSE,
    plot = FALSE,
    print = TRUE
  )
)
