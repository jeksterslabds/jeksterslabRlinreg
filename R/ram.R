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
#'   - \eqn{\boldsymbol{\beta}_{2 \cdots k}} - \eqn{p \times 1} regression slopes,
#'   - \eqn{\sigma^2} - variance of the error term
#'     \eqn{\varepsilon}, and
#'   - \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}} - \eqn{p \times p}
#'     variances and covariances of \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}.
#'
#' @family model-implied functions
#' @keywords model-implied
#' @param slopes Vector or `p` by `1` matrix.
#'   Regression slopes \eqn{\left( \boldsymbol{\beta}_{2 \cdots k} \right)} .
#' @param sigma2 Numeric.
#'   Variance of the error term \eqn{\varepsilon} \eqn{\left( \sigma^2 \right)}.
#' @param SigmaX `p` by `p` numeric matrix.
#'   Variances and covariances of \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)}.
#' @return Returns the model-implied variance-covariance matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}.
#' @export
Sigmatheta <- function(slopes,
                       sigma2,
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
  S[1, 1] <- sigma2
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
#' \eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)}
#' from parameters of a \eqn{k}-variable linear regression model.
#'
#' @details The following are the parameters of a linear regression model
#'   for the mean structure
#'   - \eqn{\boldsymbol{\beta}} - \eqn{k \times k} regression coefficients and
#'   - \eqn{\boldsymbol{\mu}_{\mathbf{X}}} - \eqn{p \times 1} vector of means of the regressors
#'     \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}} .
#'
#' @family model-implied functions
#' @keywords model-implied
#' @param beta Vector or `k` by `1` matrix.
#'   \eqn{k \times 1} regression coefficients \eqn{\left( \boldsymbol{\beta} \right)}.
#' @param muX Vector or `p` by `1` matrix. \eqn{p \times 1} vector
#'   of means of the regressors \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} \right)} .
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
