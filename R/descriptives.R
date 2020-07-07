#' Descriptive Statistics
#'
#' Calculate means, covariances and correlations.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param muhat Logical.
#'   If `TRUE`, compute means.
#' @inheritParams betahat_inv
#' @return
#' Returns a list with the following items
#'   \describe{
#'     \item{Sigmahat}{Variance-covariance matrix (\eqn{\boldsymbol{\hat{\Sigma}}}).}
#'     \item{R}{Correlation matrix (\eqn{\mathrm{R}}).}
#'     \item{VX}{Variance-covariance matrix (\eqn{\mathbf{V}_{\mathbf{X}}}).}
#'     \item{vyX}{Vector of covariances between \eqn{\mathbf{y}} and \eqn{\mathbf{X}} (\eqn{\mathbf{v}_{\mathbf{yX}}}).}
#'     \item{RX}{Correlation matrix (\eqn{\mathbf{R}_{\mathbf{X}}}).}
#'     \item{ryX}{Vector of correlations between \eqn{\mathbf{y}} and \eqn{\mathbf{X}} (\eqn{\mathbf{r}_{\mathbf{yX}}}).}
#'     \item{muhatX}{Means of regressor variables \eqn{\boldsymbol{\hat{\mu}}_\mathbf{X}}.}
#'     \item{muhaty}{Mean of regressand variable \eqn{\hat{\mu}_\mathbf{y}}.}
#'   }
#' Note that `muhatX` and `muhaty` are `NA`
#' when `muhat = FALSE`.
#' @importFrom stats cor
#' @export
descriptives <- function(X,
                         y,
                         muhat = TRUE) {
  X <- X[, -1]
  p <- ncol(X)
  yX <- cbind(y, X)
  cov <- cov(yX)
  cor <- cor(yX)
  VX <- cov[-1, -1]
  RX <- cor[-1, -1]
  vyX <- cov[, 1]
  vyX <- vyX[-1]
  ryX <- cor[, 1]
  ryX <- ryX[-1]
  if (muhat) {
    muhat <- as.vector(colMeans(yX))
    muhatX <- muhat[-1]
    muhaty <- muhat[1]
  } else {
    muhatX <- rep(x = NA, times = p)
    muhaty <- NA
  }
  list(
    Sigmahat = cov,
    R = cor,
    VX = VX,
    vyX = vyX,
    RX = RX,
    ryX = ryX,
    muhatX = muhatX,
    muhaty = muhaty
  )
}
