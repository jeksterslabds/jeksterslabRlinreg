#' ---
#' title: "Test: Beta-hat"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: Beta-hat}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ knitr_options, include=FALSE, cache=FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#+ setup
library(testthat)
library(microbenchmark)
library(jeksterslabRlinreg)
context("Test Beta-hat.")
#'
#' ## Parameters
#'
#+ parameters
n <- 100
sigma2 <- runif(
  n = 1,
  min = 9,
  max = 15
)
beta <- runif(
  n = sample(
    x = 2:5,
    size = 1
  ),
  min = 1,
  max = 3
)
Variable <- c(
  "`n`",
  "`sigma2`",
  "`beta`"
)
Description <- c(
  "Sample size ($n$).",
  "Error variance ($\\sigma^{2}_{\\epsilon}$).",
  "Regression coefficients ($\\beta$)."
)
Value <- c(
  n,
  sigma2,
  paste0(beta, collapse = ", ")
)
knitr::kable(
  x = data.frame(
    Variable,
    Description,
    Value
  ),
  row.names = FALSE
)
#'
#' ## Generate Data
#'
#+ generate_data
X <- matrix(
  data = NA,
  nrow = n,
  ncol = length(beta)
)
for (i in 1:length(beta)) {
  if (i == 1) {
    X[, i] <- rep(
      x = 1,
      times = n
    )
  } else {
    X[, i] <- runif(
      n = n,
      min = -5,
      max = 5
    )
  }
}
y <- X %*% beta + rnorm(
  n = n,
  sd = sqrt(sigma2)
)
#'
#' ## Estimate Regression Coefficients
#'
#+ estimate
result_lm <- drop(
  coef(
    lm(
      y ~ X[, -1]
    )
  )
)
result_beta_hat_inv <- beta_hat_inv(
  X = X,
  y = y
)
result_beta_hat_qr <- beta_hat_qr(
  X = X,
  y = y
)
result_beta_hat_svd <- beta_hat_svd(
  X = X,
  y = y
)
#'
#' ## Summarize Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = paste0(
      "$\\hat{\\beta}_{",
      1:length(beta),
      "}$"
    ),
    Parameter = beta,
    lm = result_lm,
    beta_hat_inv = result_beta_hat_inv,
    beta_hat_qr = result_beta_hat_qr,
    beta_hat_svd = result_beta_hat_svd
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  lm = lm(y ~ X[, -1]),
  beta_hat_inv = beta_hat_inv(X = X, y = y),
  beta_hat_qr = beta_hat_qr(X = X, y = y),
  beta_hat_svd = beta_hat_svd(X = X, y = y)
)
#'
#' ## testthat
#'
#+ testthat, echo=TRUE
test_that("beta_hat_inv, beta_hat_qr, and beta_hat_svd return the same coefficients as lm", {
  expect_equivalent(
    round(
      x = result_lm,
      digits = 2
    ),
    round(
      x = result_beta_hat_inv,
      digits = 2
    ),
    round(
      x = result_beta_hat_qr,
      digits = 2
    ),
    round(
      x = result_beta_hat_svd,
      digits = 2
    )
  )
})
