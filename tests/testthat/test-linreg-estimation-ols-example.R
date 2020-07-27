#' ---
#' title: "Tests: The Linear Regression Model (OLS)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (OLS)}
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
# The Linear Regression Model: Ordinary Least Squares {#linreg-estimation-ols-example}
#'
#+ echo = FALSE
library(microbenchmark)
library(testthat)
library(jeksterslabRlinreg)
#'
#'
#' In this example, we demonstrate how regression coefficients are estimated using ordinary least squares.
#'
#' The ordinary least squares estimator of regression slopes is given by
#'
#' \begin{equation}
#'   \boldsymbol{\hat{\beta}}
#'   =
#'   \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'   \left( \mathbf{X}^{T} \mathbf{y} \right)
#' \end{equation}
#'
#' The `jeksterslabRlinreg` package has several functions that can be used
#' in solving for $\boldsymbol{\hat{\beta}}$ namely
#'
#' - `jeksterslabRlinreg::betahatnorm()` - normal equation
#' - `jeksterslabRlinreg::betahatqr()` - using QR decomposition
#' - `jeksterslabRlinreg::betahatsvd()` - using singular value decomposition and
#' - `jeksterslabRlinreg::betahat` - calculates coefficients using the normal equation.
#'   When that fails, QR decomposition is used when `qr = TRUE`
#'   or singular value decomposition when `qr = FALSE`.
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
#'
#' ## Normal Equation
#'
#' \begin{equation}
#'   \boldsymbol{\hat{\beta}}
#'   =
#'   \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'   \left( \mathbf{X}^{T} \mathbf{y} \right)
#' \end{equation}
#'
#' The following implements the equation in `R`.
#'
#+
solve(t(X) %*% X) %*% t(X) %*% y
#'
#'
#' The `crossprod()` function is more efficient in calculating cross products.
#' Below is a more efficient implementation of the equation.
#'
#+
solve(crossprod(X), crossprod(X, y))
#'
#'
#' ### `jeksterslabRlinreg::betahatnorm()`
#'
#' The `jeksterslabRlinreg` package has a function
#' that solves for $\boldsymbol{\hat{\beta}}$ using the normal equation.
#'
#+
result_inv <- jeksterslabRlinreg::betahatnorm(
  X = X,
  y = y
)
result_inv
#'
#'
#' ## QR Decomposition
#'
#' Another way to solve for $\boldsymbol{\hat{\beta}}$ is through QR decomposition.
#'
#' The data matrix $\mathbf{X}$ is decomposed into
#'
#' \begin{equation}
#'   \mathbf{X}
#'   =
#'   \mathbf{Q}
#'   \mathbf{R}.
#' \end{equation}
#'
#' QR decomposition is implemeded in `R` using the `qr()` function.
#'
#+
Xqr <- qr(X)
R <- qr.R(Xqr)
Q <- qr.Q(Xqr)
#'
#'
#' Estimates are found by solving $\boldsymbol{\hat{\beta}}$ in
#'
#' \begin{equation}
#'   \mathbf{R}
#'   \boldsymbol{\hat{\beta}}
#'   =
#'   \mathbf{Q}^{T}
#'   \mathbf{y}.
#' \end{equation}
#'
#+
backsolve(R, crossprod(Q, y))
#'
#'
#' Another solution is to multiply both sides by the inverse of $\mathbf{R}$
#' to isolate $\boldsymbol{\hat{\beta}}$.
#'
#' \begin{equation}
#'   \boldsymbol{\hat{\beta}}
#'   =
#'   \mathbf{R}^{-1}
#'   \mathbf{Q}^{T}
#'   \mathbf{y}.
#' \end{equation}
#'
#+
solve(R) %*% crossprod(Q, y)
#'
#'
#' The first solution is more efficient.
#'
#' ### `jeksterslabRlinreg::betahatqr()`
#'
#' The `jeksterslabRlinreg` package has a function
#' that solves for $\boldsymbol{\hat{\beta}}$ using QR decomposition.
#'
#+
result_qr <- jeksterslabRlinreg::betahatqr(
  X = X,
  y = y
)
result_qr
#'
#'
#' ## Singular Value Decomposition (SVD)
#'
#' Another way to solve for $\boldsymbol{\hat{\beta}}$ is through singular value decomposition.
#'
#' The data matrix $\mathbf{X}$ is decomposed into
#'
#' \begin{equation}
#'   \mathbf{X}
#'   =
#'   \mathbf{U}
#'   \mathbf{\Sigma}
#'   \mathbf{V}^{T}.
#' \end{equation}
#'
#' $\boldsymbol{\hat{\beta}}$ is given by
#'
#' \begin{equation}
#'   \boldsymbol{\hat{\beta}}
#'   =
#'   \mathbf{V}
#'   \mathbf{\Sigma}^{+}
#'   \mathbf{U}^{T}
#'   \mathbf{y}
#' \end{equation}
#'
#' where the superscript $+$ indicates the pseudoinverse.
#'
#' Singular value decomposition is implemeded in `R` using the `svd()` function.
#'
#+
Xsvd <- svd(X)
V <- Xsvd$v
U <- Xsvd$u
Sigma <- Xsvd$d
#'
#'
#' $\boldsymbol{\hat{\beta}}$ is given by
#'
#+
V %*% ((1 / Sigma) * crossprod(U, y))
#'
#'
#' ### `jeksterslabRlinreg::betahatsvd()`
#'
#' The `jeksterslabRlinreg` package has a function
#' that solves for $\boldsymbol{\hat{\beta}}$ using singular decomposition.
#'
#+
result_svd <- jeksterslabRlinreg::betahatsvd(
  X = X,
  y = y
)
result_svd
#'
#'
#' ## `jeksterslabRlinreg::betahat()`
#'
#' `jeksterslabRlinreg::betahat()` calculates coefficients using the normal equation.
#' When that fails, QR decomposition is used when `qr = TRUE`
#' or singular value decomposition when `qr = FALSE`.
#'
#+
result_betahat <- jeksterslabRlinreg::betahat(
  X = X,
  y = y
)
#'
#'
#' ## `lm()` Function
#'
#' The `lm()` function is the default option for estimating parameters of
#' linear regression models in `R`.
#' It calculates estimates of regression coefficients and other statistics.
#'
#+
lmobj <- lm(
  y ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
result_lm <- coef(lmobj)
result_lm
summary(lmobj)
#'
#'
#' ## Why use matrix solutions over `lm()`?
#'
#' When you only need the regression coefficients you are better off
#' just calculating said coefficients using matrix operations because it is faster.
#' `lm()` calculates and provides a rich output that is very useful.
#' However, there are situations when the coefficients are enough.
#' For example, bootstrapping and simulations.
#' Having an efficient option to calculate regression coefficients is handy.
#'
#+
microbenchmark::microbenchmark(
  lm = lm(y ~ gender + race + union + education + experience, data = jeksterslabRdatarepo::wages),
  betahatnorm = jeksterslabRlinreg::betahatnorm(X = X, y = y),
  betahatqr = jeksterslabRlinreg::betahatqr(X = X, y = y),
  betahatsvd = jeksterslabRlinreg::betahatsvd(X = X, y = y)
)
#'
#'
#+
context("Test linreg-estimation-ols")
test_that("results_betahat", {
  result_lm <- as.vector(unname(result_lm))
  result_inv <- as.vector(unname(result_inv))
  result_qr <- as.vector(unname(result_qr))
  result_svd <- as.vector(unname(result_svd))
  result_betahat <- as.vector(unname(result_betahat))
  for (i in 1:length(result_lm)) {
    expect_equivalent(
      result_lm[i],
      result_inv[i]
    )
    expect_equivalent(
      result_lm[i],
      result_qr[i]
    )
    expect_equivalent(
      result_lm[i],
      result_svd[i]
    )
    expect_equivalent(
      result_lm[i],
      result_betahat[i]
    )
  }
})
test_that("X is singular.", {
  expect_error(
    jeksterslabRlinreg::betahatnorm(
      X = jeksterslabRdatarepo::svd.linreg$Xb,
      y = jeksterslabRdatarepo::svd.linreg$yb,
    )
  )
})
test_that("Using QR decomposition.", {
  expect_message(
    jeksterslabRlinreg::betahat(
      X = jeksterslabRdatarepo::svd.linreg$Xb,
      y = jeksterslabRdatarepo::svd.linreg$yb,
    ),
    "Using QR decomposition."
  )
})
test_that("Using singular value decomposition.", {
  expect_message(
    jeksterslabRlinreg::betahat(
      X = jeksterslabRdatarepo::svd.linreg$Xb,
      y = jeksterslabRdatarepo::svd.linreg$yb,
      qr = FALSE
    ),
    "Using singular value decomposition."
  )
})
#'
