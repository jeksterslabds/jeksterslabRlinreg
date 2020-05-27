#' ---
#' title: "Test: R Squared"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: R Squared}
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
context("Test R Squared.")
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
#' ## Estimate Sum of Squares
#'
#+ estimate
lm_object <- lm(
  y ~ X[, -1]
)
results_r2_lm <- summary(lm_object)$r.squared
results_rbar2_lm <- summary(lm_object)$adj.r.squared
results_rss <- rss(
  betahat = NULL,
  X = X,
  y = y
)
results_ess <- ess(
  betahat = NULL,
  X = X,
  y = y
)
results_tss <- tss(
  y = y
)
results_r2_rss <- r2(
  betahat = NULL,
  X = X,
  y = y,
  fromrss = TRUE
)
results_r2_ess <- r2(
  betahat = NULL,
  X = X,
  y = y,
  fromrss = FALSE
)
results_rbar2 <- rbar2(
  betahat = NULL,
  X = X,
  y = y
)
results_linreg <- invisible(
  linreg(
    X = X,
    y = y,
    FUN = betahat_inv,
    output = c("coef", "model", "anova")
  )
)
results_r2_linreg <- results_linreg$r2
results_rbar2_linreg <- results_linreg$rbar2
#'
#' ## Summarize Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "$R^2$",
      "$\\bar{R}^{2}$"
    ),
    lm = c(
      results_r2_lm,
      results_rbar2_lm
    ),
    r2 = c(
      results_r2_rss,
      results_rbar2
    ),
    linreg = c(
      results_r2_linreg,
      results_rbar2_linreg
    )
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  rss = r2(betahat = NULL, X = X, y = y, fromrss = TRUE),
  ess = r2(betahat = NULL, X = X, y = y, fromrss = FALSE)
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("r2", {
  expect_equivalent(
    round(
      x = results_r2_lm,
      digits = 2
    ),
    round(
      x = results_r2_rss,
      digits = 2
    ),
    round(
      x = results_r2_ess,
      digits = 2
    ),
    round(
      x = results_r2_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("rbar2", {
  expect_equivalent(
    round(
      x = results_rbar2_lm,
      digits = 2
    ),
    round(
      x = results_rbar2,
      digits = 2
    ),
    round(
      x = results_rbar2_linreg,
      digits = 2
    )
  )
})
