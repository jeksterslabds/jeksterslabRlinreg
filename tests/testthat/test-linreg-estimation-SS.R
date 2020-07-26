#' ---
#' title: "Tests: The Linear Regression Model (SS)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (SS)}
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
# The Linear Regression Model: Sum of Squares {#linreg-estimation-SS-example}
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
#' ## Residual Sum of Squares
#'
#+
epsilonhat <- epsilonhat(
  X = X,
  y = y
)
betahat <- betahat(
  X = X,
  y = y
)
result_rss1 <- .RSS(
  epsilonhat = epsilonhat
)
result_rss2 <- .RSS(
  X = X,
  y = y,
  betahat = NULL
)
result_rss3 <- .RSS(
  X = X,
  y = y,
  betahat = betahat
)
result_rss4 <- RSS(
  X = X,
  y = y
)
#'
#' ## Explained Sum of Squares
#'
#+
yhat <- yhat(
  X = X,
  y = y
)
ybar <- mean(y)
result_ess1 <- .ESS(
  yhat = yhat,
  ybar = ybar,
  betahat = NULL
)
result_ess2 <- .ESS(
  yhat = yhat,
  ybar = NULL,
  y = y
)
result_ess3 <- .ESS(
  yhat = NULL,
  ybar = ybar,
  X = X,
  y = y,
  betahat = NULL
)
result_ess4 <- .ESS(
  yhat = NULL,
  ybar = ybar,
  X = X,
  y = y,
  betahat = betahat
)
result_ess5 <- .ESS(
  yhat = NULL,
  ybar = NULL,
  X = X,
  y = y,
  betahat = betahat
)
result_ess6 <- .ESS(
  yhat = NULL,
  ybar = NULL,
  X = X,
  y = y,
  betahat = NULL
)
result_ess7 <- ESS(
  X = X,
  y = y
)
#'
#' ## Total Sum of Squares
#'
#+
result_tss <- TSS(
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
lm_anova <- anova(lmobj)
lm_RSS <- lm_anova["Residuals", "Sum Sq"]
lm_TSS <- sum(lm_anova[["Sum Sq"]])
lm_ESS <- lm_TSS - lm_RSS
#'
#'
#+
result_rss <- c(
  result_rss1, result_rss2, result_rss3, result_rss4
)
result_ess <- c(
  result_ess1, result_ess2, result_ess3, result_ess4, result_ess5, result_ess6, result_ess7
)
context("Test linreg-estimation-SS.")
test_that("RSS", {
  for (i in seq_along(result_rss)) {
    expect_equivalent(
      lm_RSS,
      result_rss[i]
    )
  }
})
test_that("ESS", {
  for (i in seq_along(result_rss)) {
    expect_equivalent(
      lm_ESS,
      result_ess[i]
    )
  }
})
test_that("TSS", {
  expect_equivalent(
    lm_TSS,
    result_tss
  )
})