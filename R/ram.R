#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Variance-Covariance Matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}
#'
#' @description Model-implied variance-covariance matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}
#'   from parameters of a \eqn{k}-variable linear regression model.
#'
#' @details The following are the parameters of a linear regression model
#'   for the covariance structure
#'   - \eqn{\boldsymbol{\beta}_{2, \cdots, k}} is the \eqn{p \times 1} column vector of regression slopes,
#'   - \eqn{\sigma_{\varepsilon}^{2}} is the variance of the error term
#'     \eqn{\varepsilon}, and
#'   - \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}} is the \eqn{p \times p} matrix of
#'     variances and covariances of \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}.
#'
#' @return Returns the model-implied variance-covariance matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}.
#'   Note that the first item corresponds to `y`.
#'   The rest of the items correspond to how `SigmaX` is arranged.
#'
#' @family model-implied functions
#' @keywords model-implied
#' @param slopes Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} column vector of regression slopes \eqn{\left( \boldsymbol{\beta}_{2, 3, \cdots, k} = \left\{ \beta_2, \beta_3, \cdots, \beta_k \right\} \right)} .
#' @param sigma2epsilon Numeric.
#'   Variance of the error term \eqn{\varepsilon} \eqn{\left( \sigma_{\varepsilon}^{2} \right)}.
#' @param SigmaX `p` by `p` numeric matrix.
#'   \eqn{p \times p} matrix of variances and covariances between regressor variables \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)}.
#' @examples
#' slopes <- c(0.207648, 0.451039)
#' sigma2epsilon <- 0.9310598
#' SigmaX <- matrix(
#'   data = c(1.2934694, 0.4379592, 0.4379592, 1.0779592),
#'   ncol = 2
#' )
#' Sigmatheta(slopes = slopes, sigma2epsilon = sigma2epsilon, SigmaX = SigmaX)
#' @export
Sigmatheta <- function(slopes,
                       sigma2epsilon,
                       SigmaX) {
  foo <- function(A,
                  S) {
    invIminusA <- solve(diag(nrow(A)) - A)
    invIminusA %*% S %*% t(invIminusA)
  }
  slopes <- as.vector(slopes)
  p <- length(slopes)
  k <- p + 1
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    slopes
  )
  filter <- diag(k)
  SigmaX[upper.tri(SigmaX)] <- 0
  S <- SigmaX
  S <- cbind(
    0,
    S
  )
  S <- rbind(
    0,
    S
  )
  S[1, 1] <- sigma2epsilon
  foo(
    A = A,
    S = S
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Mean Vector
#'   \eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)}
#'
#' @description Model-implied mean vector
#'   \eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)}
#'   from parameters of a \eqn{k}-variable linear regression model.
#'
#' @details The following are the parameters of a linear regression model
#'   for the mean structure
#'   - \eqn{\boldsymbol{\beta}} is the \eqn{k \times 1} column vector of regression coefficients, and
#'   - \eqn{\boldsymbol{\mu}_{\mathbf{X}}} is the \eqn{p \times 1} column vector of means of the regressors
#'     \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}} .
#'
#' @return Returns the model-implied mean vector
#'   \eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)}.
#'   Note that the first item corresponds to the expected value of `y`.
#'   The rest of the items correspond to `muX`.
#'
#' @family model-implied functions
#' @keywords model-implied
#' @param beta Numeric vector of length `k` or `k` by `1` matrix.
#'   \eqn{k \times 1} vector of regression coefficients \eqn{\left( \boldsymbol{\beta} \right)}.
#' @param muX Numeric vector of length `p` or `p` by `1` matrix. \eqn{p \times 1} column vector
#'   of means of the regressors \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} \right)} .
#' @examples
#' beta <- c(-12.7128845, 0.2076475, 0.4510391)
#' muX <- c(70.18, 3.06)
#' mutheta(beta = beta, muX = muX)
#' @export
mutheta <- function(beta,
                    muX) {
  foo <- function(M,
                  A) {
    solve(diag(nrow(A)) - A) %*% M
  }
  beta <- as.vector(beta)
  k <- length(beta)
  intercept <- beta[1]
  slopes <- beta[-1]
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    slopes
  )
  M <- c(
    intercept,
    as.vector(muX)
  )
  foo(
    M = M,
    A = A
  )
}
