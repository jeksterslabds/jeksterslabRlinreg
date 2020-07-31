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
result_R21 <- .R2fromRSS(
  RSS = RSS,
  y = y
)
result_R22 <- .R2fromRSS(
  TSS = TSS,
  X = X,
  y = y
)
result_R23 <- .R2fromRSS(
  RSS = RSS,
  TSS = TSS
)
result_R24 <- .R2fromRSS(
  RSS = NULL,
  TSS = NULL,
  X = X,
  y = y
)




result_R25 <- .R2fromESS(
  ESS = ESS,
  y = y
)
result_R26 <- .R2fromESS(
  TSS = TSS,
  X = X,
  y = y
)
result_R27 <- .R2fromESS(
  ESS = ESS,
  TSS = TSS
)
result_R28 <- .R2fromESS(
  X = X,
  y = y
)
result_R29 <- R2(
  X = X,
  y = y,
  fromRSS = TRUE
)
result_R210 <- R2(
  X = X,
  y = y,
  fromRSS = FALSE
)
#'
#' ## $\bar{R}^{2}$
#'
#+
result_Rbar21 <- .Rbar2(
  R2 = R2,
  n = n,
  k = k
)
result_Rbar22 <- .Rbar2(
  R2 = R2,
  n = n,
  k = k,
  fromRSS = FALSE
)
result_Rbar23 <- .Rbar2(
  X = X,
  y = y
)
result_Rbar24 <- .Rbar2(
  X = X,
  y = y,
  fromRSS = FALSE
)
result_Rbar25 <- Rbar2(
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
lm_R2 <- summary(lmobj)$r.squared
lm_Rbar2 <- summary(lmobj)$adj.r.squared
#'
#'
#+
result_R2 <- c(
  result_R21, result_R22, result_R23, result_R24, result_R25,
  result_R26, result_R27, result_R28, result_R29, result_R210
)
result_Rbar2 <- c(
  result_Rbar21, result_Rbar22, result_Rbar23,
  result_Rbar24, result_Rbar25
)
context("Test linreg-estimation-R2.")
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
