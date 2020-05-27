#' ---
#' title: "Test: vcov"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: vcov}
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
context("Test vcov.")
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
results_lm_sigma2 <- summary(lm_object)$sigma^2
results_lm_vcov <- vcov(lm_object)
# add biased tests later
results_sigma2hat_unbiased <- sigma2hat(
  X = X,
  y = y,
  type = "unbiased"
)
results_sigma2hat_biased <- sigma2hat(
  X = X,
  y = y,
  type = "biased"
)
results_sigma2hat_both <- sigma2hat(
  X = X,
  y = y,
  type = "both"
)
results_sigma2hat_both_unbiased <- results_sigma2hat_both[1]
results_sigma2hat_both_biased <- results_sigma2hat_both[2]
results_vcov_betahat_unbiased <- vcov_betahat(
  X = X,
  y = y,
  type = "unbiased"
)
results_vcov_betahat_biased <- vcov_betahat(
  X = X,
  y = y,
  type = "biased"
)
results_vcov_betahat_both <- vcov_betahat(
  X = X,
  y = y,
  type = "both"
)
results_vcov_betahat_both_unbiased <- results_vcov_betahat_both[["unbiased"]]
results_vcov_betahat_both_biased <- results_vcov_betahat_both[["biased"]]
results_linreg <- invisible(
  linreg(
    X = X,
    y = y,
    FUN = betahat_inv,
    output = c("coef", "model", "anova")
  )
)
results_sigma2hat_linreg <- results_linreg$sigma2hat
results_vcov_linreg <- results_linreg$vcov
#'
#' ## Summarize Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "$\\hat{\\sigma}^2_{\\textrm{unbiased}}$",
      "$\\hat{\\sigma}^2_{\\textrm{biased}}$"
    ),
    lm = c(
      results_lm_sigma2,
      NA
    ),
    vcov = c(
      results_sigma2hat_unbiased,
      results_sigma2hat_biased
    ),
    linreg = c(
      results_sigma2hat_linreg,
      NA
    )
  ),
  row.names = FALSE
)
knitr::kable(
  x = results_lm_vcov,
  caption = "lm vcov"
)
knitr::kable(
  x = results_vcov_betahat_unbiased,
  caption = "linreg vcov unbiased"
)
knitr::kable(
  x = results_vcov_betahat_biased,
  caption = "linreg vcov biased"
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("sigma2hat unbiased", {
  expect_equivalent(
    round(
      x = results_lm_sigma2,
      digits = 2
    ),
    round(
      x = results_sigma2hat_unbiased,
      digits = 2
    ),
    round(
      x = results_sigma2hat_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("vcov unbiased", {
  expect_equivalent(
    round(
      x = results_lm_vcov,
      digits = 2
    ),
    round(
      x = results_vcov_betahat_unbiased,
      digits = 2
    ),
    round(
      x = results_vcov_linreg,
      digits = 2
    )
  )
})
