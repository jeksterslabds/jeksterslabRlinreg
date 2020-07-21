# Structural Equation Modeling - Simple Linear Regression R Examples {#sem-simple-regression-R-examples}

library(testthat)
library(jeksterslabRlinreg)

varnames <- c("y", "x")
heights <- jeksterslabRdatarepo::heights
x <- heights$father
y <- heights$height
obj <- lm(y ~ x)
beta <- unname(coef(obj))
names(beta) <- c("beta1", "beta2")
# covariance structure
beta1 <- beta[1]
beta2 <- beta[2]
sigma2x <- var(heights$father)
sigma2y <- var(heights$height)
sigmayx <- cov(y, x)
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

result_mutheta <- jeksterslabRlinreg::mutheta(
  beta = beta,
  muX = mux
)

colnames(result_mutheta) <- "$\\mu$"
rownames(result_mutheta) <- varnames
knitr::kable(
  x = result_mutheta,
  caption = "Expected Values"
)

result_Sigmatheta <- jeksterslabRlinreg::Sigmatheta(
  slopes = beta2,
  sigma2 = sigma2epsilon,
  SigmaX = sigma2x
)

colnames(result_Sigmatheta) <- varnames
rownames(result_Sigmatheta) <- varnames
knitr::kable(
  x = result_Sigmatheta,
  caption = "Covariance Expectations"
)

result_slopes <- jeksterslabRlinreg::slopes(
  SigmaX = sigma2x,
  sigmayX = sigmayx
)
result_slopes
result_intercept <- jeksterslabRlinreg::intercept(
  slopes = result_slopes,
  muy = muy,
  muX = mux
)
result_intercept

result_sigma2 <- sigma2(
  slopes = beta2,
  sigma2y = sigma2y,
  sigmayX = sigmayx,
  SigmaX = sigma2x
)
result_sigma2

context("Test model-implied")
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
