#' ---
#' title: "Test: y-hat (y_hat)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: y-hat (y_hat)}
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
context("Test y-hat (y_hat).")
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
#' ## Calculate $\mathbf{\hat{y}}$
#'
#+ estimate
lm_object <- lm(
  y ~ X[, -1]
)
lm_coeff <- coef(lm_object)
results_y_hat_lm <- predict.lm(
  object = lm_object,
  newdata = as.data.frame(X[, -1])
)
results_y_hat_Py <- y_hat_Py(
  y = y,
  P = NULL,
  X = X
)
P <- proj_P(
  X = X
)
results_y_hat_Py_P <- y_hat_Py(
  y = y,
  P = P,
  X = NULL
)
results_y_hat_Xbeta_hat <- y_hat_Xbeta_hat(
  X = X,
  beta_hat = NULL,
  y = y
)
beta_hat <- beta_hat_inv(
  X = X,
  y = y
)
results_y_hat_Xbeta_hat_beta_hat <- y_hat_Xbeta_hat(
  X = X,
  beta_hat = beta_hat,
  y = NULL
)
results_y_hat <- y_hat(
  X = X,
  y = y,
  beta_hat = NULL
)
results_y_hat_beta_hat <- y_hat(
  X = X,
  y = NULL,
  beta_hat = beta_hat
)
results_y_hat_test <- y_hat_test(
  beta_hat = beta_hat,
  X = X,
  y = y
)
#'
#' ## Summarize Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Case = 1:nrow(X),
    lm = results_y_hat_lm,
    y_hat_Py = results_y_hat_Py,
    y_hat_Py_P = results_y_hat_Py_P,
    y_hat_Xbeta_hat = results_y_hat_Xbeta_hat,
    y_hat_Xbeta_hat_beta_hat = results_y_hat_Xbeta_hat_beta_hat,
    y_hat = results_y_hat,
    y_hat_beta_hat = results_y_hat_beta_hat,
    y_hat_test = results_y_hat_test[["y_hat"]]
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  lm = predict.lm(object = lm_object, newdata = as.data.frame(X[, -1])),
  y_hat_Py = y_hat_Py(y = y, P = NULL, X = X),
  y_hat_Py_P = y_hat_Py(y = y, P = P, X = NULL),
  y_hat_Xbeta_hat = y_hat_Xbeta_hat(X = X, beta_hat = NULL, y = y),
  y_hat_Xbeta_hat_beta_hat = y_hat_Xbeta_hat(X = X, beta_hat = beta_hat, y = NULL),
  y_hat = y_hat(X = X, y = y, beta_hat = NULL),
  y_hat_beta_hat = y_hat(X = X, y = NULL, beta_hat = beta_hat),
  y_hat_test = y_hat_test(beta_hat = beta_hat, X = X, y = y)
)
#'
#' ## testthat
#'
#+ testthat, echo=TRUE
test_that("y_hat_Py, y_hat_Xbeta_hat, and y_hat return the same values as predict.lm(object = lm_object, newdata = X[, -1])", {
  expect_equivalent(
    round(
      x = results_y_hat_lm,
      digits = 2
    ),
    round(
      x = results_y_hat_Py,
      digits = 2
    ),
    round(
      x = results_y_hat_Py_P,
      digits = 2
    ),
    round(
      x = results_y_hat_Xbeta_hat,
      digits = 2
    ),
    round(
      x = results_y_hat_Xbeta_hat_beta_hat,
      digits = 2
    ),
    round(
      x = results_y_hat,
      digits = 2
    ),
    round(
      x = results_y_hat_beta_hat,
      digits = 2
    ),
    round(
      x = results_y_hat_test[["y_hat"]],
      digits = 2
    )
  )
})
