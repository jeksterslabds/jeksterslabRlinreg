#' ---
#' title: "Test: linreg"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: linreg}
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
context("Test linreg.")
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
lm_object <- lm(
  y ~ X[, -1]
)
result_lm <- drop(
  coef(
    lm_object
  )
)
lm_anova <- anova(lm_object)
results_linreg <- linreg(
  X = X,
  y = y,
  FUN = betahat_inv,
  output = c("coef", "model", "anova")
)
results_betahat_linreg <- results_linreg$betahat
results_se_linreg <- results_linreg$se
results_t_linreg <- results_linreg$t
results_p_linreg <- results_linreg$p
results_df1_linreg <- results_linreg$df1
results_df2_linreg <- results_linreg$df2
results_rss_linreg <- results_linreg$rss
results_ess_linreg <- results_linreg$ess
results_ms_model_linreg <- results_linreg$ms_model
results_ms_error_linreg <- results_linreg$ms_error
results_F_linreg <- results_linreg$F
results_Fp_linreg <- results_linreg$Fp
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
    betahat_linreg = results_betahat_linreg
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  lm = lm(y ~ X[, -1]),
  linreg = linreg(X = X, y = y, FUN = betahat_inv, output = NULL)
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("linreg estimates compared to lm", {
  expect_equivalent(
    round(
      x = summary(lm(y ~ X[, -1]))$coefficients[, "Estimate"],
      digits = 2
    ),
    round(
      x = results_betahat_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("linreg se compared to lm", {
  expect_equivalent(
    round(
      x = summary(lm(y ~ X[, -1]))$coefficients[, "Std. Error"],
      digits = 2
    ),
    round(
      x = results_se_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_03, echo=TRUE
test_that("linreg t compared to lm", {
  expect_equivalent(
    round(
      x = summary(lm(y ~ X[, -1]))$coefficients[, "t value"],
      digits = 2
    ),
    round(
      x = results_t_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_04, echo=TRUE
test_that("linreg p compared to lm", {
  expect_equivalent(
    round(
      x = summary(lm(y ~ X[, -1]))$coefficients[, "Pr(>|t|)"],
      digits = 2
    ),
    round(
      x = results_p_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_05, echo=TRUE
test_that("linreg df1 compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Df"][1, 1],
      digits = 2
    ),
    round(
      x = results_df1_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_06, echo=TRUE
test_that("linreg df2 compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Df"][2, 1],
      digits = 2
    ),
    round(
      x = results_df2_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_07, echo=TRUE
test_that("linreg ess compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Sum Sq"][1, 1],
      digits = 2
    ),
    round(
      x = results_ess_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_08, echo=TRUE
test_that("linreg rss compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Sum Sq"][2, 1],
      digits = 2
    ),
    round(
      x = results_rss_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_09, echo=TRUE
test_that("linreg ms_model compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Mean Sq"][1, 1],
      digits = 2
    ),
    round(
      x = results_ms_model_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_10, echo=TRUE
test_that("linreg ms_error compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Mean Sq"][2, 1],
      digits = 2
    ),
    round(
      x = results_ms_error_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_11, echo=TRUE
test_that("linreg F compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["F value"][1, 1],
      digits = 2
    ),
    round(
      x = results_F_linreg,
      digits = 2
    )
  )
})
#'
#+ testthat_12, echo=TRUE
test_that("linreg Fp compared to lm", {
  expect_equivalent(
    round(
      x = lm_anova["Pr(>F)"][1, 1],
      digits = 2
    ),
    round(
      x = results_Fp_linreg,
      digits = 2
    )
  )
})
