#' ---
#' title: "Test: y-hat (yhat)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: y-hat (yhat)}
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
context("Test y-hat (yhat).")
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
results_yhat_lm <- predict.lm(
  object = lm_object,
  newdata = as.data.frame(X[, -1])
)
results_Py <- Py(
  y = y,
  P = NULL,
  X = X
)
P <- P(
  X = X
)
results_Py_P <- Py(
  y = y,
  P = P,
  X = NULL
)
results_Xbetahat <- Xbetahat(
  X = X,
  betahat = NULL,
  y = y
)
betahat <- betahat_inv(
  X = X,
  y = y
)
results_Xbetahat_betahat <- Xbetahat(
  X = X,
  betahat = betahat,
  y = NULL
)
results_yhat <- yhat(
  X = X,
  y = y,
  betahat = NULL
)
results_yhat_betahat <- yhat(
  X = X,
  y = NULL,
  betahat = betahat
)
results_yhat_test <- yhat_test(
  betahat = betahat,
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
    lm = results_yhat_lm,
    Py = results_Py,
    Py_P = results_Py_P,
    Xbetahat = results_Xbetahat,
    Xbetahat_betahat = results_Xbetahat_betahat,
    yhat = results_yhat,
    yhat_betahat = results_yhat_betahat,
    yhat_test = results_yhat_test[["yhat"]]
  ),
  row.names = FALSE
)
#'
#' ## Benchmarking
#'
#+ benchmark
microbenchmark(
  lm = predict.lm(object = lm_object, newdata = as.data.frame(X[, -1])),
  Py = Py(y = y, P = NULL, X = X),
  Py_P = Py(y = y, P = P, X = NULL),
  Xbetahat = Xbetahat(X = X, betahat = NULL, y = y),
  Xbetahat_betahat = Xbetahat(X = X, betahat = betahat, y = NULL),
  yhat = yhat(X = X, y = y, betahat = NULL),
  yhat_betahat = yhat(X = X, y = NULL, betahat = betahat),
  yhat_test = yhat_test(betahat = betahat, X = X, y = y)
)
#'
#' ## testthat
#'
#+ testthat, echo=TRUE
test_that("Py, Xbetahat, and yhat return the same values as predict.lm(object = lm_object, newdata = X[, -1])", {
  expect_equivalent(
    round(
      x = results_yhat_lm,
      digits = 2
    ),
    round(
      x = results_Py,
      digits = 2
    ),
    round(
      x = results_Py_P,
      digits = 2
    ),
    round(
      x = results_Xbetahat,
      digits = 2
    ),
    round(
      x = results_Xbetahat_betahat,
      digits = 2
    ),
    round(
      x = results_yhat,
      digits = 2
    ),
    round(
      x = results_yhat_betahat,
      digits = 2
    ),
    round(
      x = results_yhat_test[["yhat"]],
      digits = 2
    )
  )
})
