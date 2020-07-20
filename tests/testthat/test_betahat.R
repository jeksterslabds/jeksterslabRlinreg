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
result_lm_object <- lm(
  y ~ X[, -1]
)
result_lm_betahat <- drop(
  coef(
    result_lm_object
  )
)
result_betahatinv <- betahatinv(
  X = X,
  y = y
)
result_betahatqr <- betahatqr(
  X = X,
  y = y
)
result_betahatsvd <- betahatsvd(
  X = X,
  y = y
)
result_betahat_betahatinv <- betahat(
  X = X,
  y = y,
  FUN = betahatinv
)
result_betahat_betahatqr <- betahat(
  X = X,
  y = y,
  FUN = betahatqr
)
result_betahat_betahatsvd <- betahat(
  X = X,
  y = y,
  FUN = betahatsvd
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
    lm = result_lm_betahat,
    betahatinv = result_betahatinv,
    betahatqr = result_betahatqr,
    betahatsvd = result_betahatsvd,
    betahat_betahatinv = result_betahat_betahatinv,
    betahat_betahatqr = result_betahat_betahatqr,
    betahat_betahatsvd = result_betahat_betahatsvd
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  lm = coef(lm(y ~ X[, -1])),
  betahatinv = betahatinv(X = X, y = y),
  betahatqr = betahatqr(X = X, y = y),
  betahatsvd = betahatsvd(X = X, y = y),
  betahat_betahatinv = betahat(X = X, y = y, FUN = betahatinv),
  betahat_betahatqr = betahat(X = X, y = y, FUN = betahatqr),
  betahat_betahatsvd = betahat(X = X, y = y, FUN = betahatsvd)
)
#'
#' ## testthat
#'
#+ testthat, echo=TRUE
test_that("betahatinv, betahatqr, betahatsvd, and betahat return the same coefficients as lm", {
  expect_equivalent(
    round(
      x = result_lm_betahat,
      digits = 2
    ),
    round(
      x = result_betahatinv,
      digits = 2
    ),
    round(
      x = result_betahatqr,
      digits = 2
    ),
    round(
      x = result_betahatsvd,
      digits = 2
    ),
    round(
      x = result_betahat_betahatinv,
      digits = 2
    ),
    round(
      x = result_betahat_betahatqr,
      digits = 2
    ),
    round(
      x = result_betahat_betahatsvd,
      digits = 2
    )
  )
})
