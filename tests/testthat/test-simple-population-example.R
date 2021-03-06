#' ---
#' title: "Tests: The Simple Linear Regression Model"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Simple Linear Regression Model}
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
# The Simple Linear Regression Model: The Population Regression Model Example {#simple-population-example}
#'
#+ echo = FALSE
library(testthat)
library(jeksterslabRlinreg)
#'
#'
#+ echo = FALSE
varnames <- c("y", "x")
wages <- jeksterslabRdatarepo::wages
x <- wages$education
y <- wages$wages
n <- length(x)
obj <- lm(y ~ x)
beta <- unname(coef(obj))
names(beta) <- c("beta1", "beta2")
# covariance structure
beta1 <- beta[1]
beta2 <- beta[2]
sigma2x <- var(x)
sigma2y <- var(y)
# sigma^2 has some discrepancy
# sigma2epsilon <- summary(obj)$sigma^2
sigma2epsilon <- sigma2y - (beta2^2 * sigma2x)
# mean structure
mux <- mean(x)
#'
#'
#+ echo = FALSE
x <- rnorm(n = n, mean = mux, sd = sqrt(sigma2x))
epsilon <- rnorm(n = n, mean = 0, sd = sqrt(sigma2epsilon))
y <- beta1 + beta2 * x + epsilon
obj <- lm(y ~ x)
beta <- unname(coef(obj))
names(beta) <- c("beta1", "beta2")
# covariance structure
beta1 <- beta[1]
beta2 <- beta[2]
sigma2x <- var(x)
sigma2y <- var(y)
sigmayx <- cov(y, x)
ryx <- cor(y, x)
Sigmatheta <- matrix(
  data = c(
    sigma2y,
    sigmayx,
    sigmayx,
    sigma2x
  ),
  nrow = 2,
  ncol = 2
)
# sigma^2 has some discrepancy
# sigma2epsilon <- summary(obj)$sigma^2
sigma2epsilon <- sigma2y - (beta2^2 * sigma2x)
# mean structure
mux <- mean(x)
muy <- mean(y)
mutheta <- matrix(
  data = c(muy, mux),
  ncol = 1
)
X <- cbind(
  constant = 1,
  education = x
)
y <- matrix(
  data = y,
  ncol = 1
)
colnames(y) <- "wages"
#'
#'
#' The `jeksterslabRlinreg` package has functions to derive expectations for regression models.
#'
#' - `jeksterslabRlinreg::mutheta()` returns the expected values,
#'   that is, the model-implied mean vector.
#' - `jeksterslabRlinreg::Sigmatheta()` returns the covariance expectations,
#'   that is, the model-implied variance-covariance matrix.
#'
#' In this hypothetical example,
#' we assume that we have population data
#' and we are interested in the association between wages and education.
#' The regressor variable is years of education.
#' The regressand variable is hourly wage in US dollars.
#' The intercept is the predicted wage of an employee with 0 years of education.
#' The slope is the increase in hourly wage in US dollars
#' for one year increase in education.
#'
#+ echo = FALSE
plot(x = x, y = y, xlab = "Years of Education", ylab = "Hourly Wages in US Dollars")
abline(obj, col = "red")
#'
#'
#' The the following vectors represent the parameters of the simple linear regression model.
#'
#' \begin{equation}
#'   \boldsymbol{\theta}_{\text{mean structure}}
#'   =
#'   \begin{bmatrix}
#'     \beta_1 \\
#'     \mu_x
#'   \end{bmatrix}
#' \end{equation}
#'
#' \begin{equation}
#'   \boldsymbol{\theta}_{\text{covariance structure}}
#'   =
#'   \begin{bmatrix}
#'     \beta_2 \\
#'     \sigma_{\varepsilon}^{2} \\
#'     \sigma_{x}^{2}
#'   \end{bmatrix}
#' \end{equation}
#'
#'
#+ echo = FALSE
Variable <- c(
  "`beta1`",
  "`mux`"
)
Description <- c(
  "Intercept",
  "Mean of $x$"
)
Notation <- c(
  "$\\beta_1$",
  "$\\mu_x$"
)
Value <- c(
  beta1,
  mux
)
knitr::kable(
  x = data.frame(
    Variable,
    Description,
    Notation,
    Value
  ),
  caption = "$\\boldsymbol{\\theta}_{\\text{mean structure}}$",
  row.names = FALSE
)
#'
#'
#+ echo = FALSE
Variable <- c(
  "`beta2`",
  "`sigma2epsilon`",
  "`sigma2x`"
)
Description <- c(
  "Slope",
  "Variance of $\\varepsilon$",
  "Variance of $x$"
)
Notation <- c(
  "$\\beta_2$",
  "$\\sigma_{\\varepsilon}^{2}$",
  "$\\sigma_{x}^{2}$"
)
Value <- c(
  beta2,
  sigma2epsilon,
  sigma2x
)
knitr::kable(
  x = data.frame(
    Variable,
    Description,
    Notation,
    Value
  ),
  caption = "$\\boldsymbol{\\theta}_{\\text{covariance structure}}$",
  row.names = FALSE
)
#'
#'
#' There are times when the intercept does not lend to a meaningful interpretation.
#' A negative wage corresponding to zero years of education
#' does not really make a lot of sense.
#' The value that is interesting in this case is the slope.
#' The slope represents `r beta2` US dollar increase in hourly wages
#' given 1 additional year of education.
#'
#' The expected values are derived using `jeksterslabRlinreg::mutheta()`.
#'
#+
result_mutheta <- jeksterslabRlinreg::mutheta(
  beta = beta,
  muX = mux
)
#'
#'
#+ echo = FALSE
colnames(result_mutheta) <- "$\\mu$"
rownames(result_mutheta) <- varnames
knitr::kable(
  x = result_mutheta,
  caption = "Expected Values"
)
#'
#'
#' The covariance expectations are derive using `jeksterslabRlinreg::Sigmatheta()`.
#'
#+
result_Sigmatheta <- jeksterslabRlinreg::Sigmatheta(
  slopes = beta2,
  sigma2epsilon = sigma2epsilon,
  SigmaX = sigma2x
)
#'
#'
#+ echo = FALSE
colnames(result_Sigmatheta) <- varnames
rownames(result_Sigmatheta) <- varnames
knitr::kable(
  x = result_Sigmatheta,
  caption = "Covariance Expectations"
)
#'
#'
#' The inverse, that is, deriving the parameters from the expectations
#' can also be done using functions from the `jeksterslabRlinreg` package.
#'
#' The regression coefficients can be derived using
#' `jeksterslabRlinreg::intercept()`, and `jeksterslabRlinreg::slopes()`
#' from the means and the covariances.
#'
#+
result_slopes <- jeksterslabRlinreg::.slopes(
  SigmaX = sigma2x,
  sigmayX = sigmayx
)
result_slopes
result_intercept <- jeksterslabRlinreg::.intercept(
  slopes = result_slopes,
  muy = muy,
  muX = mux
)
result_intercept
#'
#' From raw data
#+
result_slopesfromrawdata <- jeksterslabRlinreg::slopes(
  X = X,
  y = y
)
result_slopesfromrawdata
result_interceptfromrawdata <- jeksterslabRlinreg::intercept(
  X = X,
  y = y
)
result_interceptfromrawdata
#'
#'
#' Standardized slopes can also be obtained using `jeksterslabRlinreg::slopesprime()`
#'
#+
result_slopesprime <- jeksterslabRlinreg::.slopesprime(
  RX = sqrt(sigma2x),
  ryX = ryx
)
result_slopesprime
#'
#' From raw data
#'
#+
result_slopesprimefromrawdata <- jeksterslabRlinreg::slopesprime(
  X = X,
  y = y
)
result_slopesprimefromrawdata
#'
#'
#' The error variance can also be derived with the available information using
#' `jeksterslabRlinreg::sigma2()` .
#'
#+
result_sigma2epsilon <- sigma2epsilon(
  slopes = beta2,
  sigma2y = sigma2y,
  sigmayX = sigmayx,
  SigmaX = sigma2x
)
result_sigma2epsilon
#'
#'
#+
context("Test simple-regression-ram")
test_that("result_mutheta", {
  for (i in 1:nrow(result_mutheta)) {
    expect_equivalent(
      result_mutheta[i, 1],
      mutheta[i, 1]
    )
  }
})
test_that("result_Sigmatheta", {
  for (i in 1:nrow(result_Sigmatheta)) {
    for (j in 1:ncol(result_Sigmatheta)) {
      expect_equivalent(
        result_Sigmatheta[i, j],
        Sigmatheta[i, j]
      )
    }
  }
})
test_that("results_beta", {
  results_beta <- c(
    result_intercept,
    result_slopes
  )
  for (i in 1:length(results_beta)) {
    expect_equivalent(
      results_beta[i],
      beta[i]
    )
  }
})
test_that("results_betafromrawdata", {
  results_betafromrawdata <- c(
    result_interceptfromrawdata,
    result_slopesfromrawdata
  )
  for (i in 1:length(results_betafromrawdata)) {
    expect_equivalent(
      results_betafromrawdata[i],
      beta[i]
    )
  }
})
