#' ---
#' title: "Test: Sum of Squares"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: Sum of Squares}
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
context("Test Sum of Squares.")
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
lm_coeff <- coef(lm_object)
lm_anova <- anova(lm_object)
results_ess_lm <- lm_anova[["Sum Sq"]][1]
results_rss_lm <- lm_anova[["Sum Sq"]][2]
results_tss_lm <- results_ess_lm + results_rss_lm
results_r_sqr_lm <- summary(lm_object)$r.squared
results_r_bar_sqr_lm <- summary(lm_object)$adj.r.squared
results_ss_r <- ss_r(
  beta_hat = NULL,
  X = X,
  y = y
)
results_ss_e <- ss_e(
  beta_hat = NULL,
  X = X,
  y = y
)
results_ss_t <- ss_t(
  y = y
)
results_r_sqr_rss <- r_sqr(
  beta_hat = NULL,
  X = X,
  y = y,
  rss = TRUE
)
results_r_sqr_ess <- r_sqr(
  beta_hat = NULL,
  X = X,
  y = y,
  rss = FALSE
)
results_r_bar_sqr_rss <- r_bar_sqr(
  r2 = NULL,
  n = nrow(X),
  k = ncol(X),
  X = X,
  y = y,
  rss = TRUE
)
results_r_bar_sqr_ess <- r_bar_sqr(
  r2 = NULL,
  n = nrow(X),
  k = ncol(X),
  X = X,
  y = y,
  rss = FALSE
)
#'
#' ## Summarize Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c("$RSS$", "$ESS$", "$TSS$", "$R^2$", "$\\bar{R}^{2}$"),
    lm = c(results_rss_lm, results_ess_lm, results_tss_lm, results_r_sqr_lm, results_r_bar_sqr_lm),
    linreg = c(results_ss_r, results_ss_e, results_ss_t, results_r_sqr_rss, results_r_bar_sqr_rss)
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  rss = r_sqr(beta_hat = NULL, X = X, y = y, rss = TRUE),
  ess = r_sqr(beta_hat = NULL, X = X, y = y, rss = FALSE)
)
microbenchmark(
  rss = r_bar_sqr(r2 = NULL, n = nrow(X), k = ncol(X), X = X, y = y, rss = TRUE),
  ess = r_bar_sqr(r2 = NULL, n = nrow(X), k = ncol(X), X = X, y = y, rss = FALSE)
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("rss", {
  expect_equivalent(
    round(
      x = results_rss_lm,
      digits = 2
    ),
    round(
      x = results_ss_r,
      digits = 2
    )
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("ess", {
  expect_equivalent(
    round(
      x = results_ess_lm,
      digits = 2
    ),
    round(
      x = results_ss_e,
      digits = 2
    )
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("tss", {
  expect_equivalent(
    round(
      x = results_tss_lm,
      digits = 2
    ),
    round(
      x = results_ss_t,
      digits = 2
    )
  )
})
#'
#+ testthat_04, echo=TRUE
test_that("r_sqr", {
  expect_equivalent(
    round(
      x = results_r_sqr_lm,
      digits = 2
    ),
    round(
      x = results_r_sqr_rss,
      digits = 2
    ),
    round(
      x = results_r_sqr_ess,
      digits = 2
    )
  )
})
#'
#+ testthat_05, echo=TRUE
test_that("r_bar_sqr", {
  expect_equivalent(
    round(
      x = results_r_bar_sqr_lm,
      digits = 2
    ),
    round(
      x = results_r_bar_sqr_rss,
      digits = 2
    ),
    round(
      x = results_r_bar_sqr_ess,
      digits = 2
    )
  )
})
